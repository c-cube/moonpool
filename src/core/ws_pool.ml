open Types_
module A = Atomic
module WSQ = Ws_deque_
module WL = Worker_loop_
include Runner

let ( let@ ) = ( @@ )

type state = {
  active: bool A.t;  (** Becomes [false] when the pool is shutdown. *)
  mutable workers: worker_state array;  (** Fixed set of workers. *)
  main_q: WL.task_full Queue.t;
      (** Main queue for tasks coming from the outside *)
  mutable idle: worker_state list;
      (** Parked workers, protected by mutex. LIFO: waking the most recently
          parked worker keeps the others asleep. *)
  n_idle: int A.t;  (** Length of [idle], written under mutex, read freely *)
  mutex: Mutex.t;
  mutable as_runner: t;
  (* init options *)
  name: string option;
  on_init_thread: dom_id:int -> t_id:int -> unit -> unit;
  on_exit_thread: dom_id:int -> t_id:int -> unit -> unit;
  on_exn: exn -> Printexc.raw_backtrace -> unit;
}
(** internal state *)

and worker_state = {
  mutable thread: Thread.t;
  idx: int;
  mutable dom_id: int;  (** Set before the thread starts *)
  st: state;
  q: WL.task_full WSQ.t;  (** Work stealing queue *)
  rng: Random.State.t;
  parker: Parker_.t;  (** Used to block/unblock the worker thread reliably *)
  mutable is_idle: bool;  (** In [st.idle], protected by [st.mutex] *)
}
(** State for a given worker. Only this worker is allowed to push into the
    queue, but other workers can come and steal from it if they're idle. *)

let[@inline] size_ (self : state) = Array.length self.workers

let num_tasks_ (self : state) : int =
  let n = ref 0 in
  n := Queue.length self.main_q;
  Array.iter (fun w -> n := !n + WSQ.size w.q) self.workers;
  !n

(** TLS, used by worker to store their specific state and be able to retrieve it
    from tasks when we schedule new sub-tasks. *)
let k_worker_state : worker_state TLS.t = TLS.create ()

let[@inline] get_current_worker_ () : worker_state option =
  TLS.get_opt k_worker_state

(** Take an idle worker. Precondition: we hold the mutex. The caller must unpark
    it. *)
let pop_idle_ (self : state) : worker_state option =
  match self.idle with
  | [] -> None
  | w :: tl ->
    self.idle <- tl;
    A.decr self.n_idle;
    w.is_idle <- false;
    Some w

(** Precondition: we hold the mutex and [w.is_idle = true]. *)
let remove_idle_ (w : worker_state) : unit =
  w.st.idle <- List.filter (fun w' -> w != w') w.st.idle;
  A.decr w.st.n_idle;
  assert (A.get w.st.n_idle = List.length w.st.idle);
  w.is_idle <- false

let[@inline] unpark_worker (w : worker_state) : unit = Parker_.unpark w.parker

(** Wake up an idle worker, if there's any. *)
let wake_one_ (self : state) : unit =
  if A.get self.n_idle > 0 then (
    Mutex.lock self.mutex;
    let w = pop_idle_ self in
    Mutex.unlock self.mutex;
    Option.iter unpark_worker w
  )

(** Push into worker's local queue, open to work stealing. precondition: this
    runs on the worker thread whose state is [self] *)
let schedule_on_current_worker (self : worker_state) task : unit =
  (* we're on this same pool, schedule in the worker's state. Otherwise
     we might also be on pool A but asking to schedule on pool B,
     so we have to check that identifiers match. *)
  let pushed = WSQ.push self.q task in
  if pushed then
    wake_one_ self.st
  else (
    (* overflow into main queue *)
    Mutex.lock self.st.mutex;
    Queue.push task self.st.main_q;
    (* wake up one idle worker, if any *)
    let w = pop_idle_ self.st in
    Mutex.unlock self.st.mutex;
    Option.iter unpark_worker w
  )

(** Push into the shared queue of this pool *)
let schedule_in_main_queue (self : state) task : unit =
  (* check [active] under the lock: workers only exit after seeing it false
     with an empty [main_q] under this lock, so a task pushed here is run *)
  Mutex.lock self.mutex;
  if not (A.get self.active) then (
    Mutex.unlock self.mutex;
    (* notify the caller that scheduling tasks is no
       longer permitted *)
    raise Shutdown
  );
  Queue.push task self.main_q;
  let w = pop_idle_ self in
  Mutex.unlock self.mutex;
  Option.iter unpark_worker w

let schedule_from_anywhere_ (st : state) (task : WL.task_full) : unit =
  match get_current_worker_ () with
  | Some w when st == w.st ->
    (* use worker from the same pool *)
    schedule_on_current_worker w task
  | _ -> schedule_in_main_queue st task

let schedule_from_w (w : worker_state) task : unit =
  schedule_from_anywhere_ w.st task

exception Got_task of WL.task_full

(** Try to steal a task. *)
let try_to_steal_work_once_ (self : worker_state) : WL.task_full option =
  let init = Random.State.int self.rng (Array.length self.st.workers) in
  try
    for i = 0 to Array.length self.st.workers - 1 do
      let w' =
        Array.unsafe_get self.st.workers
          ((i + init) mod Array.length self.st.workers)
      in

      (* no self-stealing! *)
      if self != w' then (
        match WSQ.steal w'.q with
        | Some t -> raise_notrace (Got_task t)
        | None -> ()
      )
    done;
    None
  with Got_task t -> Some t

let rec get_next_task (self : worker_state) : WL.task_full =
  (* see if we can empty the local queue *)
  match WSQ.pop_exn self.q with
  | task ->
    if WSQ.size self.q > 0 then wake_one_ self.st;
    task
  | exception WSQ.Empty -> try_to_steal_from_other_workers_ self

and try_to_steal_from_other_workers_ (self : worker_state) =
  match try_to_steal_work_once_ self with
  | Some task -> task
  | None -> wait_on_main_queue self

and wait_on_main_queue (self : worker_state) : WL.task_full =
  Mutex.lock self.st.mutex;
  match Queue.pop self.st.main_q with
  | task ->
    Mutex.unlock self.st.mutex;
    task
  | exception Queue.Empty ->
    if not (A.get self.st.active) then (
      (* shutting down, exit. Tasks overflowing into [main_q] from now on are
         still run by the remaining workers, with less parallelism: they come
         from a running worker, which drains [main_q] before exiting. *)
      Mutex.unlock self.st.mutex;
      raise WL.No_more_tasks
    );

    (* register as idle to be sure not to miss a new main-queue task *)
    self.is_idle <- true;
    self.st.idle <- self :: self.st.idle;
    A.incr self.st.n_idle;
    Mutex.unlock self.st.mutex;

    (* try to steal a task anyway, in case one was made available in the mean
       time. Must come after registering as idle. *)
    (match try_to_steal_work_once_ self with
    | Some task ->
      Mutex.lock self.st.mutex;
      (* not waiting on main queue anymore *)
      let is_idle = self.is_idle in
      if is_idle then remove_idle_ self;
      Mutex.unlock self.st.mutex;
      if not is_idle then (
        (* a waker picked us for its task, pass the wakeup on *)
        Parker_.park self.parker;
        wake_one_ self.st
      );
      task
    | None ->
      (* just gotta wait *)
      Parker_.park self.parker;
      get_next_task self)

let before_start (self : worker_state) : unit =
  let t_id = Thread.id @@ Thread.self () in
  self.st.on_init_thread ~dom_id:self.dom_id ~t_id ();
  TLS.set k_cur_fiber _dummy_fiber;
  TLS.set Runner.For_runner_implementors.k_cur_runner self.st.as_runner;
  TLS.set k_worker_state self;

  (* set thread name *)
  Option.iter
    (fun name ->
      Tracing_.set_thread_name (Printf.sprintf "%s.worker.%d" name self.idx))
    self.st.name

let cleanup (self : worker_state) : unit =
  (* on termination, decrease refcount of underlying domain *)
  Domain_pool_.decr_on self.dom_id;
  let t_id = Thread.id @@ Thread.self () in
  self.st.on_exit_thread ~dom_id:self.dom_id ~t_id ()

let worker_ops : worker_state WL.ops =
  let runner (st : worker_state) = st.st.as_runner in
  let on_exn (st : worker_state) (ebt : Exn_bt.t) =
    st.st.on_exn (Exn_bt.exn ebt) (Exn_bt.bt ebt)
  in
  {
    WL.schedule = schedule_from_w;
    runner;
    get_next_task;
    on_exn;
    before_start;
    cleanup;
  }

let shutdown_ ~wait (self : state) : unit =
  if A.exchange self.active false then (
    Mutex.lock self.mutex;
    let idle = self.idle in
    self.idle <- [];
    A.set self.n_idle 0;
    List.iter (fun w -> w.is_idle <- false) idle;
    Mutex.unlock self.mutex;
    List.iter unpark_worker idle
  );
  if wait then Array.iter (fun w -> Thread.join w.thread) self.workers

let as_runner_ (self : state) : t =
  Runner.For_runner_implementors.create
    ~shutdown:(fun ~wait () -> shutdown_ self ~wait)
    ~run_async:(fun ~fiber f ->
      let task = WL.T_start { fiber; f } in
      schedule_from_anywhere_ self task)
    ~size:(fun () -> size_ self)
    ~num_tasks:(fun () -> num_tasks_ self)
    ()

type ('a, 'b) create_args = ('a, 'b) Util_pool_.create_args
(** Arguments used in {!create}. See {!create} for explanations. *)

let create ?(on_init_thread = Util_pool_.default_thread_init_exit_)
    ?(on_exit_thread = Util_pool_.default_thread_init_exit_)
    ?(on_exn = Util_pool_.on_exn) ?num_threads ?name () : t =
  let num_threads = Util_pool_.num_threads ?num_threads () in

  let pool =
    {
      active = A.make true;
      workers = [||];
      main_q = Queue.create ();
      idle = [];
      n_idle = A.make 0;
      mutex = Mutex.create ();
      on_exn;
      on_init_thread;
      on_exit_thread;
      name;
      as_runner = Runner.dummy;
    }
  in
  pool.as_runner <- as_runner_ pool;

  (* build worker states first, then start threads. this way workers do
    not see a dummy state *)
  pool.workers <-
    Array.init num_threads (fun idx ->
        {
          st = pool;
          thread = Thread.self ();
          q = WSQ.create ~dummy:WL._dummy_task ();
          rng = Random.State.make [| idx |];
          dom_id = 0;
          idx;
          parker = Parker_.create ();
          is_idle = false;
        });

  (* start the thread for worker [idx] (on domain [dom_id]) *)
  let mk_thread idx ~dom_id : Thread.t =
    let w = pool.workers.(idx) in
    w.dom_id <- dom_id;
    let thread =
      Thread.create (WL.worker_loop ~block_signals:true ~ops:worker_ops) w
    in
    w.thread <- thread;
    thread
  in

  ignore
    (Util_pool_.spawn_workers_round_robin ~num_threads mk_thread
      : Thread.t array);

  pool.as_runner

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?num_threads ?name () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?num_threads ?name ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
