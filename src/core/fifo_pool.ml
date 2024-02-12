open Types_
include Runner

let ( let@ ) = ( @@ )
let k_storage = Task_local_storage.Private_.Storage.k_storage

type task_full = {
  f: unit -> unit;
  name: string;
  ls: Task_local_storage.storage;
}

type state = {
  threads: Thread.t array;
  q: task_full Bb_queue.t;  (** Queue for tasks. *)
}
(** internal state *)

let[@inline] size_ (self : state) = Array.length self.threads
let[@inline] num_tasks_ (self : state) : int = Bb_queue.size self.q

(** Run [task] as is, on the pool. *)
let schedule_ (self : state) (task : task_full) : unit =
  try Bb_queue.push self.q task with Bb_queue.Closed -> raise Shutdown

type around_task = AT_pair : (t -> 'a) * (t -> 'a -> unit) -> around_task

let worker_thread_ (self : state) (runner : t) ~on_exn ~around_task : unit =
  let cur_ls : Task_local_storage.storage ref = ref Task_local_storage.Private_.Storage.dummy in
  TLS.set k_storage (Some cur_ls);
  TLS.get Runner.For_runner_implementors.k_cur_runner := Some runner;

  let (AT_pair (before_task, after_task)) = around_task in

  let cur_span = ref Tracing_.dummy_span in

  let[@inline] exit_span_ () =
    Tracing_.exit_span !cur_span;
    cur_span := Tracing_.dummy_span
  in

  let on_suspend () =
    exit_span_ ();
    !cur_ls
  in

  let run_another_task ls ~name task' =
    let ls' = Task_local_storage.Private_.Storage.copy ls in
    schedule_ self { f = task'; name; ls = ls' }
  in

  let run_task (task : task_full) : unit =
    cur_ls := task.ls;
    let _ctx = before_task runner in
    cur_span := Tracing_.enter_span task.name;

    let resume ls k res =
      schedule_ self { f = (fun () -> k res); name = task.name; ls }
    in

    (* run the task now, catching errors, handling effects *)
    (try
[@@@ifge 5.0]
      Suspend_.with_suspend (WSH {
        run=run_another_task;
        resume;
        on_suspend;
      }) task.f
[@@@else_]
      task.f()
[@@@endif]
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       on_exn e bt);
    exit_span_ ();
    after_task runner _ctx;
    cur_ls := Task_local_storage.Private_.Storage.dummy
  in

  let main_loop () =
    let continue = ref true in
    while !continue do
      assert (!cur_span = Tracing_.dummy_span);
      match Bb_queue.pop self.q with
      | task -> run_task task
      | exception Bb_queue.Closed -> continue := false
    done
  in

  try
    (* handle domain-local await *)
    Dla_.using ~prepare_for_await:Suspend_.prepare_for_await
      ~while_running:main_loop
  with Bb_queue.Closed -> ()

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let shutdown_ ~wait (self : state) : unit =
  Bb_queue.close self.q;
  if wait then Array.iter Thread.join self.threads

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?num_threads:int ->
  ?name:string ->
  'a

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
    ?around_task ?num_threads ?name () : t =
  (* wrapper *)
  let around_task =
    match around_task with
    | Some (f, g) -> AT_pair (f, g)
    | None -> AT_pair (ignore, fun _ _ -> ())
  in

  let num_domains = Domain_pool_.n_domains () in

  (* number of threads to run *)
  let num_threads = Util_pool_.num_threads ?num_threads () in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int num_domains in

  let pool =
    let dummy = Thread.self () in
    { threads = Array.make num_threads dummy; q = Bb_queue.create () }
  in

  let run_async ~name ~ls f = schedule_ pool { f; name; ls } in

  let runner =
    Runner.For_runner_implementors.create
      ~shutdown:(fun ~wait () -> shutdown_ pool ~wait)
      ~run_async
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

      (* set thread name *)
      Option.iter
        (fun name ->
          Tracing_.set_thread_name (Printf.sprintf "%s.worker.%d" name i))
        name;

      let run () = worker_thread_ pool runner ~on_exn ~around_task in

      (* now run the main loop *)
      Fun.protect run ~finally:(fun () ->
          (* on termination, decrease refcount of underlying domain *)
          Domain_pool_.decr_on dom_idx);
      on_exit_thread ~dom_id:dom_idx ~t_id ()
    in

    (* function called in domain with index [i], to
       create the thread and push it into [receive_threads] *)
    let create_thread_in_domain () =
      let thread = Thread.create main_thread_fun () in
      (* send the thread from the domain back to us *)
      Bb_queue.push receive_threads (i, thread)
    in

    Domain_pool_.run_on dom_idx create_thread_in_domain
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

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?num_threads
    ?name () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?num_threads
      ?name ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
