open Types_
include Runner

let ( let@ ) = ( @@ )
let k_storage = Task_local_storage.Private_.Storage.k_storage

type task_full = {
  f: unit -> unit;
  ls: Task_local_storage.storage;
}

type state = {
  mutable bg_thread: Thread.t;
  q: task_full Bb_queue.t;  (** Queue for tasks. *)
}
(** internal state *)

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

  let run_another_task ls task' =
    let ls' = Task_local_storage.Private_.Storage.copy ls in
    schedule_ self { f = task'; ls = ls' }
  in

  let run_task (task : task_full) : unit =
    cur_ls := task.ls;
    let _ctx = before_task runner in

    let resume ls k res =
      schedule_ self { f = (fun () -> k res);  ls }
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
    let local = Queue.create() in
    while !continue do
      assert (!cur_span = Tracing_.dummy_span);
      match Bb_queue.transfer self.q local with
      | () ->
        while not (Queue.is_empty local) do
          let task = Queue.pop local in
          run_task task
        done
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
  if wait then Thread.join self.bg_thread

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?name:string ->
  'a

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
    ?around_task ?name () : t =
  (* wrapper *)
  let around_task =
    match around_task with
    | Some (f, g) -> AT_pair (f, g)
    | None -> AT_pair (ignore, fun _ _ -> ())
  in

  let num_domains = Domain_pool_.n_domains () in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let dom_idx = Random.int num_domains in

  let pool =
    let dummy = Thread.self () in
    { bg_thread = dummy; q = Bb_queue.create () }
  in

  let run_async ~ls f = schedule_ pool { f; ls } in

  let runner =
    Runner.For_runner_implementors.create
      ~shutdown:(fun ~wait () -> shutdown_ pool ~wait)
      ~run_async
      ~size:(fun () ->1)
      ~num_tasks:(fun () -> num_tasks_ pool)
      ()
  in

  let start_thread () : Thread.t =
    (* function run in the thread itself *)
    let main_thread_fun () : unit =
      let thread = Thread.self () in
      let t_id = Thread.id thread in
      on_init_thread ~dom_id:dom_idx ~t_id ();

      (* set thread name *)
      Option.iter
        (fun name ->
          Tracing_.set_thread_name (Printf.sprintf "%s.worker" name))
        name;

      let run () = worker_thread_ pool runner ~on_exn ~around_task in

      (* now run the main loop *)
      Fun.protect run ~finally:(fun () ->
          (* on termination, decrease refcount of underlying domain *)
          Domain_pool_.decr_on dom_idx);
      on_exit_thread ~dom_id:dom_idx ~t_id ()
    in
    Thread.create main_thread_fun ()
  in

  let bg_thread = Domain_pool_.run_on_and_wait dom_idx start_thread  in
  pool.bg_thread <- bg_thread;

  runner

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?around_task 
    ?name () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?around_task 
      ?name ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
