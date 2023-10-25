module WSQ = Ws_deque_
include Runner

let ( let@ ) = ( @@ )

type thread_loop_wrapper =
  thread:Thread.t -> pool:t -> (unit -> unit) -> unit -> unit

type worker_state = {
  mutable thread: Thread.t;
  q: task WSQ.t;  (** Work stealing queue *)
}

type state = {
  workers: worker_state array;
  main_q: task Bb_queue.t;  (** Main queue to block on *)
}
(** internal state *)

let[@inline] size_ (self : state) = Array.length self.workers

let num_tasks_ (self : state) : int =
  let n = ref (Bb_queue.size self.main_q) in
  Array.iter (fun w -> n := !n + WSQ.size w.q) self.workers;
  !n

exception Got_worker of worker_state

let find_current_worker_ (self : state) : worker_state option =
  let self_id = Thread.id @@ Thread.self () in
  try
    (* see if we're in one of the worker threads *)
    for i = 0 to Array.length self.workers - 1 do
      let w = self.workers.(i) in
      if Thread.id w.thread = self_id then raise_notrace (Got_worker w)
    done;
    None
  with Got_worker w -> Some w

(** Run [task] as is, on the pool. *)
let run_direct_ (self : state) (w : worker_state option) (task : task) : unit =
  match w with
  | Some w ->
    print_endline "push local";
    WSQ.push w.q task
  | None ->
    print_endline "push blocking";
    Bb_queue.push self.main_q task

let run_async_ (self : state) (task : task) : unit =
  (* stay on current worker if possible *)
  let w = find_current_worker_ self in

  let rec run_async_rec_ (task : task) =
    let task_with_suspend_ () =
      (* run [f()] and handle [suspend] in it *)
      Suspend_.with_suspend task ~run:(fun ~with_handler task' ->
          if with_handler then
            run_async_rec_ task'
          else
            run_direct_ self w task')
    in
    run_direct_ self w task_with_suspend_
  in
  run_async_rec_ task

let run = run_async

exception Got_task of task

type around_task = AT_pair : (t -> 'a) * (t -> 'a -> unit) -> around_task

(** How many times in a row do we try to read the next local task? *)
let run_self_task_max_retry = 5

(** How many times in a row do we try to do work-stealing? *)
let steal_attempt_max_retry = 5

let worker_thread_ (self : state) (runner : t) (w : worker_state) ~on_exn
    ~around_task : unit =
  let (AT_pair (before_task, after_task)) = around_task in

  (* run this task. *)
  let run_task task : unit =
    let _ctx = before_task runner in
    (* run the task now, catching errors *)
    (try task ()
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       on_exn e bt);
    after_task runner _ctx
  in

  let run_self_tasks_ () =
    print_endline "run self tasks";
    let continue = ref true in
    let pop_retries = ref 0 in
    while !continue do
      match WSQ.pop w.q with
      | Some task ->
        pop_retries := 0;
        run_task task
      | None ->
        Domain_.relax ();
        incr pop_retries;
        if !pop_retries > run_self_task_max_retry then continue := false
    done
  in

  (* get a task from another worker *)
  let try_to_steal_work () : task option =
    print_endline "try to steal work";
    try
      for _retry = 1 to 3 do
        Array.iter
          (fun w' ->
            if w != w' then (
              match WSQ.steal w'.q with
              | None -> ()
              | Some task -> raise_notrace (Got_task task)
            ))
          self.workers
      done;
      None
    with Got_task task -> Some task
  in

  (* try to steal work multiple times *)
  let try_to_steal_work_loop () : bool =
    try
      let unsuccessful_steal_attempts = ref 0 in
      while !unsuccessful_steal_attempts < steal_attempt_max_retry do
        match try_to_steal_work () with
        | Some task ->
          run_task task;
          raise_notrace Exit
        | None ->
          incr unsuccessful_steal_attempts;
          Domain_.relax ()
      done;
      false
    with Exit -> true
  in

  let main_loop () =
    (try
       while true do
         run_self_tasks_ ();

         if not (try_to_steal_work_loop ()) then (
           Array.iteri
             (fun i w -> Printf.printf "w[%d].q.size=%d\n" i (WSQ.size w.q))
             self.workers;
           Printf.printf "bq.size=%d\n%!" (Bb_queue.size self.main_q);

           print_endline "wait block";
           let task = Bb_queue.pop self.main_q in
           run_task task
         )
       done
     with Bb_queue.Closed -> ());
    run_self_tasks_ ()
  in

  (* handle domain-local await *)
  Dla_.using ~prepare_for_await:Suspend_.prepare_for_await
    ~while_running:main_loop

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let shutdown_ ~wait (self : state) : unit =
  Bb_queue.close self.main_q;
  if wait then Array.iter (fun w -> Thread.join w.thread) self.workers

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?thread_wrappers:thread_loop_wrapper list ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?min:int ->
  ?per_domain:int ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(thread_wrappers = [])
    ?(on_exn = fun _ _ -> ()) ?around_task ?min:(min_threads = 1)
    ?(per_domain = 0) () : t =
  (* wrapper *)
  let around_task =
    match around_task with
    | Some (f, g) -> AT_pair (f, g)
    | None -> AT_pair (ignore, fun _ _ -> ())
  in

  (* number of threads to run *)
  let min_threads = max 1 min_threads in
  let num_domains = D_pool_.n_domains () in
  assert (num_domains >= 1);
  let num_threads = max min_threads (num_domains * per_domain) in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int num_domains in

  let workers : worker_state array =
    let dummy = Thread.self () in
    Array.init num_threads (fun _ -> { thread = dummy; q = WSQ.create () })
  in

  let pool = { workers; main_q = Bb_queue.create () } in

  let runner =
    Runner.For_runner_implementors.create
      ~shutdown:(fun ~wait () -> shutdown_ pool ~wait)
      ~run_async:(fun f -> run_async_ pool f)
      ~size:(fun () -> size_ pool)
      ~num_tasks:(fun () -> num_tasks_ pool)
      ()
  in

  (* temporary queue used to obtain thread handles from domains
     on which the thread are started. *)
  let receive_threads = Bb_queue.create () in

  (* start the thread with index [i] *)
  let start_thread_with_idx i =
    let w = pool.workers.(i) in
    let dom_idx = (offset + i) mod num_domains in

    (* function run in the thread itself *)
    let main_thread_fun () : unit =
      let thread = Thread.self () in
      let t_id = Thread.id thread in
      on_init_thread ~dom_id:dom_idx ~t_id ();

      let run () = worker_thread_ pool runner w ~on_exn ~around_task in
      (* the actual worker loop is [worker_thread_], with all
         wrappers for this pool and for all pools (global_thread_wrappers_) *)
      let run' =
        List.fold_left
          (fun run f -> f ~thread ~pool:runner run)
          run thread_wrappers
      in

      (* now run the main loop *)
      Fun.protect run' ~finally:(fun () ->
          (* on termination, decrease refcount of underlying domain *)
          D_pool_.decr_on dom_idx);
      on_exit_thread ~dom_id:dom_idx ~t_id ()
    in

    (* function called in domain with index [i], to
       create the thread and push it into [receive_threads] *)
    let create_thread_in_domain () =
      let thread = Thread.create main_thread_fun () in
      (* send the thread from the domain back to us *)
      Bb_queue.push receive_threads (i, thread)
    in

    D_pool_.run_on dom_idx create_thread_in_domain
  in

  (* start all threads, placing them on the domains
     according to their index and [offset] in a round-robin fashion. *)
  for i = 0 to num_threads - 1 do
    start_thread_with_idx i
  done;

  (* receive the newly created threads back from domains *)
  for _j = 1 to num_threads do
    let i, th = Bb_queue.pop receive_threads in
    pool.workers.(i).thread <- th
  done;

  runner

let with_ ?on_init_thread ?on_exit_thread ?thread_wrappers ?on_exn ?around_task
    ?min ?per_domain () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?thread_wrappers ?on_exn ?around_task
      ?min ?per_domain ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
