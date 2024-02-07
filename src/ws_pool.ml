module WSQ = Ws_deque_
module A = Atomic_
module TLS = Thread_local_storage_
include Runner

let ( let@ ) = ( @@ )

module Id = struct
  type t = unit ref
  (** Unique identifier for a pool *)

  let create () : t = Sys.opaque_identity (ref ())
  let equal : t -> t -> bool = ( == )
end

type task_with_name = {
  f: task;
  name: string;
}

type worker_state = {
  pool_id_: Id.t;  (** Unique per pool *)
  mutable thread: Thread.t;
  q: task_with_name WSQ.t;  (** Work stealing queue *)
  mutable cur_span: int64;
  rng: Random.State.t;
}
(** State for a given worker. Only this worker is
    allowed to push into the queue, but other workers
    can come and steal from it if they're idle. *)

type around_task = AT_pair : (t -> 'a) * (t -> 'a -> unit) -> around_task

type state = {
  id_: Id.t;
  active: bool A.t;  (** Becomes [false] when the pool is shutdown. *)
  workers: worker_state array;  (** Fixed set of workers. *)
  main_q: task_with_name Queue.t;
      (** Main queue for tasks coming from the outside *)
  mutable n_waiting: int; (* protected by mutex *)
  mutable n_waiting_nonzero: bool;  (** [n_waiting > 0] *)
  mutex: Mutex.t;
  cond: Condition.t;
  on_exn: exn -> Printexc.raw_backtrace -> unit;
  around_task: around_task;
}
(** internal state *)

let[@inline] size_ (self : state) = Array.length self.workers

let num_tasks_ (self : state) : int =
  let n = ref 0 in
  n := Queue.length self.main_q;
  Array.iter (fun w -> n := !n + WSQ.size w.q) self.workers;
  !n

(** TLS, used by worker to store their specific state
    and be able to retrieve it from tasks when we schedule new
    sub-tasks. *)
let k_worker_state : worker_state option ref TLS.key =
  TLS.new_key (fun () -> ref None)

let[@inline] find_current_worker_ () : worker_state option =
  !(TLS.get k_worker_state)

(** Try to wake up a waiter, if there's any. *)
let[@inline] try_wake_someone_ (self : state) : unit =
  if self.n_waiting_nonzero then (
    Mutex.lock self.mutex;
    Condition.signal self.cond;
    Mutex.unlock self.mutex
  )

(** Run [task] as is, on the pool. *)
let schedule_task_ (self : state) ~name (w : worker_state option) (f : task) :
    unit =
  (* Printf.printf "schedule task now (%d)\n%!" (Thread.id @@ Thread.self ()); *)
  let task = { f; name } in
  match w with
  | Some w when Id.equal self.id_ w.pool_id_ ->
    (* we're on this same pool, schedule in the worker's state. Otherwise
       we might also be on pool A but asking to schedule on pool B,
       so we have to check that identifiers match. *)
    let pushed = WSQ.push w.q task in
    if pushed then
      try_wake_someone_ self
    else (
      (* overflow into main queue *)
      Mutex.lock self.mutex;
      Queue.push task self.main_q;
      if self.n_waiting_nonzero then Condition.signal self.cond;
      Mutex.unlock self.mutex
    )
  | _ ->
    if A.get self.active then (
      (* push into the main queue *)
      Mutex.lock self.mutex;
      Queue.push task self.main_q;
      if self.n_waiting_nonzero then Condition.signal self.cond;
      Mutex.unlock self.mutex
    ) else
      (* notify the caller that scheduling tasks is no
         longer permitted *)
      raise Shutdown

(** Run this task, now. Must be called from a worker. *)
let run_task_now_ (self : state) ~runner (w : worker_state) ~name task : unit =
  (* Printf.printf "run task now (%d)\n%!" (Thread.id @@ Thread.self ()); *)
  let (AT_pair (before_task, after_task)) = self.around_task in
  let _ctx = before_task runner in

  w.cur_span <- Tracing_.enter_span name;
  let[@inline] exit_span_ () =
    Tracing_.exit_span w.cur_span;
    w.cur_span <- Tracing_.dummy_span
  in

  let run_another_task ~name task' =
    let w = find_current_worker_ () in
    schedule_task_ self w ~name task'
  in

  (* run the task now, catching errors *)
  (try
     (* run [task()] and handle [suspend] in it *)
     Suspend_.with_suspend task ~name ~run:run_another_task
       ~on_suspend:exit_span_
   with e ->
     let bt = Printexc.get_raw_backtrace () in
     self.on_exn e bt);
  exit_span_ ();
  after_task runner _ctx

let[@inline] run_async_ (self : state) ~name (f : task) : unit =
  let w = find_current_worker_ () in
  schedule_task_ self w ~name f

(* TODO: function to schedule many tasks from the outside.
    - build a queue
    - lock
    - queue transfer
    - wakeup all (broadcast)
    - unlock *)

(** Wait on condition. Precondition: we hold the mutex. *)
let[@inline] wait_ (self : state) : unit =
  self.n_waiting <- self.n_waiting + 1;
  if self.n_waiting = 1 then self.n_waiting_nonzero <- true;
  Condition.wait self.cond self.mutex;
  self.n_waiting <- self.n_waiting - 1;
  if self.n_waiting = 0 then self.n_waiting_nonzero <- false

exception Got_task of task_with_name

(** Try to steal a task *)
let try_to_steal_work_once_ (self : state) (w : worker_state) :
    task_with_name option =
  let init = Random.State.int w.rng (Array.length self.workers) in

  try
    for i = 0 to Array.length self.workers - 1 do
      let w' =
        Array.unsafe_get self.workers ((i + init) mod Array.length self.workers)
      in

      if w != w' then (
        match WSQ.steal w'.q with
        | Some t -> raise_notrace (Got_task t)
        | None -> ()
      )
    done;
    None
  with Got_task t -> Some t

(** Worker runs tasks from its queue until none remains *)
let worker_run_self_tasks_ (self : state) ~runner w : unit =
  let continue = ref true in
  while !continue && A.get self.active do
    match WSQ.pop w.q with
    | Some task ->
      try_wake_someone_ self;
      run_task_now_ self ~runner w ~name:task.name task.f
    | None -> continue := false
  done

(** Main loop for a worker thread. *)
let worker_thread_ (self : state) ~(runner : t) (w : worker_state) : unit =
  TLS.get Runner.For_runner_implementors.k_cur_runner := Some runner;
  TLS.get k_worker_state := Some w;

  let rec main () : unit =
    worker_run_self_tasks_ self ~runner w;
    try_steal ()
  and run_task task : unit =
    run_task_now_ self ~runner w ~name:task.name task.f;
    main ()
  and try_steal () =
    match try_to_steal_work_once_ self w with
    | Some task -> run_task task
    | None -> wait ()
  and wait () =
    Mutex.lock self.mutex;
    match Queue.pop self.main_q with
    | task ->
      Mutex.unlock self.mutex;
      run_task task
    | exception Queue.Empty ->
      (* wait here *)
      if A.get self.active then (
        wait_ self;

        (* see if a task became available *)
        let task =
          try Some (Queue.pop self.main_q) with Queue.Empty -> None
        in
        Mutex.unlock self.mutex;

        match task with
        | Some t -> run_task t
        | None -> try_steal ()
      ) else
        (* do nothing more: no task in main queue, and we are shutting
           down so no new task should arrive.
           The exception is if another task is creating subtasks
           that overflow into the main queue, but we can ignore that at
           the price of slightly decreased performance for the last few
           tasks *)
        Mutex.unlock self.mutex
  in

  (* handle domain-local await *)
  Dla_.using ~prepare_for_await:Suspend_.prepare_for_await ~while_running:main

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let shutdown_ ~wait (self : state) : unit =
  if A.exchange self.active false then (
    Mutex.lock self.mutex;
    Condition.broadcast self.cond;
    Mutex.unlock self.mutex;
    if wait then Array.iter (fun w -> Thread.join w.thread) self.workers
  )

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?num_threads:int ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

let dummy_task_ = { f = ignore; name = "DUMMY_TASK" }

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
    ?around_task ?num_threads () : t =
  let pool_id_ = Id.create () in
  (* wrapper *)
  let around_task =
    match around_task with
    | Some (f, g) -> AT_pair (f, g)
    | None -> AT_pair (ignore, fun _ _ -> ())
  in

  let num_domains = D_pool_.n_domains () in
  let num_threads = Util_pool_.num_threads ?num_threads () in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int num_domains in

  let workers : worker_state array =
    let dummy = Thread.self () in
    Array.init num_threads (fun i ->
        {
          pool_id_;
          thread = dummy;
          cur_span = Tracing_.dummy_span;
          q = WSQ.create ~dummy:dummy_task_ ();
          rng = Random.State.make [| i |];
        })
  in

  let pool =
    {
      id_ = pool_id_;
      active = A.make true;
      workers;
      main_q = Queue.create ();
      n_waiting = 0;
      n_waiting_nonzero = true;
      mutex = Mutex.create ();
      cond = Condition.create ();
      around_task;
      on_exn;
    }
  in

  let runner =
    Runner.For_runner_implementors.create
      ~shutdown:(fun ~wait () -> shutdown_ pool ~wait)
      ~run_async:(fun ~name f -> run_async_ pool ~name f)
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

      let run () = worker_thread_ pool ~runner w in

      (* now run the main loop *)
      Fun.protect run ~finally:(fun () ->
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
    let worker_state = pool.workers.(i) in
    worker_state.thread <- th
  done;

  runner

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?num_threads () f
    =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?num_threads ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
