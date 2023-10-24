(* TODO: use a better queue for the tasks *)

module A = Atomic_
include Runner

let ( let@ ) = ( @@ )

(** Thread safe queue, non blocking *)
module TS_queue = struct
  type 'a t = {
    mutex: Mutex.t;
    q: 'a Queue.t;
  }

  let create () : _ t = { mutex = Mutex.create (); q = Queue.create () }

  let try_push (self : _ t) x : bool =
    if Mutex.try_lock self.mutex then (
      Queue.push x self.q;
      Mutex.unlock self.mutex;
      true
    ) else
      false

  let push (self : _ t) x : unit =
    Mutex.lock self.mutex;
    Queue.push x self.q;
    Mutex.unlock self.mutex

  let try_pop ~force_lock (self : _ t) : _ option =
    let has_lock =
      if force_lock then (
        Mutex.lock self.mutex;
        true
      ) else
        Mutex.try_lock self.mutex
    in
    if has_lock then (
      match Queue.pop self.q with
      | x ->
        Mutex.unlock self.mutex;
        Some x
      | exception Queue.Empty ->
        Mutex.unlock self.mutex;
        None
    ) else
      None
end

type thread_loop_wrapper =
  thread:Thread.t -> pool:t -> (unit -> unit) -> unit -> unit

let global_thread_wrappers_ : thread_loop_wrapper list A.t = A.make []

let add_global_thread_loop_wrapper f : unit =
  while
    let l = A.get global_thread_wrappers_ in
    not (A.compare_and_set global_thread_wrappers_ l (f :: l))
  do
    Domain_.relax ()
  done

type state = {
  active: bool A.t;
  threads: Thread.t array;
  qs: task TS_queue.t array;
  num_tasks: int A.t;
  mutex: Mutex.t;
  cond: Condition.t;
  cur_q: int A.t;  (** Selects queue into which to push *)
}
(** internal state *)

let[@inline] size_ (self : state) = Array.length self.threads
let[@inline] num_tasks_ (self : state) : int = A.get self.num_tasks

let awake_workers_ (self : state) : unit =
  Mutex.lock self.mutex;
  Condition.broadcast self.cond;
  Mutex.unlock self.mutex

(** Run [task] as is, on the pool. *)
let run_direct_ (self : state) (task : task) : unit =
  let n_qs = Array.length self.qs in
  let offset = A.fetch_and_add self.cur_q 1 in

  (* push that forces lock acquisition, last resort *)
  let[@inline] push_wait f =
    let q_idx = offset mod Array.length self.qs in
    let q = self.qs.(q_idx) in
    TS_queue.push q f
  in

  (try
     (* try each queue with a round-robin initial offset *)
     for _retry = 1 to 10 do
       for i = 0 to n_qs - 1 do
         let q_idx = (i + offset) mod Array.length self.qs in
         let q = self.qs.(q_idx) in

         if TS_queue.try_push q task then raise_notrace Exit
       done
     done;
     push_wait task
   with Exit -> ());

  (* successfully pushed, now see if we need to wakeup workers *)
  let old_num_tasks = A.fetch_and_add self.num_tasks 1 in
  if old_num_tasks < size_ self then awake_workers_ self

let rec run_async_ (self : state) (task : task) : unit =
  let task' () =
    (* run [f()] and handle [suspend] in it *)
    Suspend_.with_suspend task ~run:(fun ~with_handler task ->
        if with_handler then
          run_async_ self task
        else
          run_direct_ self task)
  in
  run_direct_ self task'

let run = run_async

[@@@ifge 5.0]

(* DLA interop *)
let prepare_for_await () : Dla_.t =
  (* current state *)
  let st :
      ((with_handler:bool -> task -> unit) * Suspend_.suspension) option A.t =
    A.make None
  in

  let release () : unit =
    match A.exchange st None with
    | None -> ()
    | Some (run, k) -> run ~with_handler:true (fun () -> k (Ok ()))
  and await () : unit =
    Suspend_.suspend
      { Suspend_.handle = (fun ~run k -> A.set st (Some (run, k))) }
  in

  let t = { Dla_.release; await } in
  t

[@@@else_]

let prepare_for_await () = { Dla_.release = ignore; await = ignore }

[@@@endif]

exception Got_task of task

type around_task = AT_pair : (t -> 'a) * (t -> 'a -> unit) -> around_task

exception Closed

let worker_thread_ (self : state) (runner : t) ~on_exn ~around_task
    ~(offset : int) : unit =
  let num_qs = Array.length self.qs in
  let (AT_pair (before_task, after_task)) = around_task in

  (* try to get a task that is already in one of the queues.
     @param force_lock if true, we force acquisition of the queue's mutex,
      which is slower but always succeeds to get a task if there's one. *)
  let get_task_already_in_queues ~force_lock () : _ option =
    try
      for _retry = 1 to 3 do
        for i = 0 to num_qs - 1 do
          let q = self.qs.((offset + i) mod num_qs) in
          match TS_queue.try_pop ~force_lock q with
          | Some f -> raise_notrace (Got_task f)
          | None -> ()
        done
      done;
      None
    with Got_task f ->
      A.decr self.num_tasks;
      Some f
  in

  (* slow path: force locking when trying to get tasks,
     and wait on [self.cond] if no task is currently available. *)
  let pop_blocking () : task =
    try
      while A.get self.active do
        match get_task_already_in_queues ~force_lock:true () with
        | Some t -> raise_notrace (Got_task t)
        | None ->
          Mutex.lock self.mutex;
          (* NOTE: be careful about race conditions: we must only
             block if the [shutdown] that sets [active] to [false]
             has not broadcast over this condition first. Otherwise
             we might miss the signal and wait here forever. *)
          if A.get self.active then Condition.wait self.cond self.mutex;
          Mutex.unlock self.mutex
      done;
      raise Closed
    with Got_task t -> t
  in

  let run_task task : unit =
    let _ctx = before_task runner in
    (* run the task now, catching errors *)
    (try task ()
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       on_exn e bt);
    after_task runner _ctx
  in

  (* drain the queues from existing tasks. If [force_lock=false]
      then it is best effort. *)
  let run_tasks_already_present ~force_lock () =
    let continue = ref true in
    while !continue do
      match get_task_already_in_queues ~force_lock () with
      | None -> continue := false
      | Some task -> run_task task
    done
  in

  let main_loop () =
    while A.get self.active do
      run_tasks_already_present ~force_lock:false ();

      (* no task available, block until one comes *)
      match pop_blocking () with
      | exception Closed -> ()
      | task -> run_task task
    done;

    (* cleanup *)
    run_tasks_already_present ~force_lock:true ()
  in

  try
    (* handle domain-local await *)
    Dla_.using ~prepare_for_await ~while_running:main_loop
  with Bb_queue.Closed -> ()

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

(** We want a reasonable number of queues. Even if your system is
    a beast with hundreds of cores, trying
    to work-steal through hundreds of queues will have a cost.

    Hence, we limit the number of queues to at most 32 (number picked
    via the ancestral technique of the pifomètre). *)
let max_queues = 32

let shutdown_ ~wait (self : state) : unit =
  let was_active = A.exchange self.active false in
  (* wake up the subset of [self.threads] that are waiting on new tasks *)
  if was_active then awake_workers_ self;
  if wait then Array.iter Thread.join self.threads

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

  let active = A.make true in
  let qs =
    let num_qs = min (min num_domains num_threads) max_queues in
    Array.init num_qs (fun _ -> TS_queue.create ())
  in

  let pool =
    let dummy = Thread.self () in
    {
      active;
      threads = Array.make num_threads dummy;
      num_tasks = A.make 0;
      qs;
      mutex = Mutex.create ();
      cond = Condition.create ();
      cur_q = A.make 0;
    }
  in

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
    let dom_idx = (offset + i) mod num_domains in

    (* function run in the thread itself *)
    let main_thread_fun () : unit =
      let thread = Thread.self () in
      let t_id = Thread.id thread in
      on_init_thread ~dom_id:dom_idx ~t_id ();

      let all_wrappers =
        List.rev_append thread_wrappers (A.get global_thread_wrappers_)
      in

      let run () = worker_thread_ pool runner ~on_exn ~around_task ~offset:i in
      (* the actual worker loop is [worker_thread_], with all
         wrappers for this pool and for all pools (global_thread_wrappers_) *)
      let run' =
        List.fold_left
          (fun run f -> f ~thread ~pool:runner run)
          run all_wrappers
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
    pool.threads.(i) <- th
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
