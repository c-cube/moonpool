open Types_
module A = Atomic_
module WSQ = Ws_deque_
module WL = Worker_loop_
include Runner

let ( let@ ) = ( @@ )

module Id = struct
  type t = unit ref
  (** Unique identifier for a pool *)

  let create () : t = Sys.opaque_identity (ref ())
  let equal : t -> t -> bool = ( == )
end

type state = {
  id_: Id.t;
      (** Unique to this pool. Used to make sure tasks stay within the same pool. *)
  active: bool A.t;  (** Becomes [false] when the pool is shutdown. *)
  mutable workers: worker_state array;  (** Fixed set of workers. *)
  main_q: WL.task_full Queue.t;
      (** Main queue for tasks coming from the outside *)
  mutable n_waiting: int; (* protected by mutex *)
  mutable n_waiting_nonzero: bool;  (** [n_waiting > 0] *)
  mutex: Mutex.t;
  cond: Condition.t;
  mutable as_runner: t;
  (* init options *)
  around_task: WL.around_task;
  name: string option;
  on_init_thread: dom_id:int -> t_id:int -> unit -> unit;
  on_exit_thread: dom_id:int -> t_id:int -> unit -> unit;
  on_exn: exn -> Printexc.raw_backtrace -> unit;
}
(** internal state *)

and worker_state = {
  mutable thread: Thread.t;
  idx: int;
  dom_id: int;
  st: state;
  q: WL.task_full WSQ.t;  (** Work stealing queue *)
  rng: Random.State.t;
}
(** State for a given worker. Only this worker is
    allowed to push into the queue, but other workers
    can come and steal from it if they're idle. *)

let[@inline] size_ (self : state) = Array.length self.workers

let num_tasks_ (self : state) : int =
  let n = ref 0 in
  n := Queue.length self.main_q;
  Array.iter (fun w -> n := !n + WSQ.size w.q) self.workers;
  !n

(** TLS, used by worker to store their specific state
    and be able to retrieve it from tasks when we schedule new
    sub-tasks. *)
let k_worker_state : worker_state TLS.t = TLS.create ()

let[@inline] get_current_worker_ () : worker_state option =
  TLS.get_opt k_worker_state

let[@inline] get_current_worker_exn () : worker_state =
  match TLS.get_exn k_worker_state with
  | w -> w
  | exception TLS.Not_set ->
    failwith "Moonpool: get_current_runner was called from outside a pool."

(** Try to wake up a waiter, if there's any. *)
let[@inline] try_wake_someone_ (self : state) : unit =
  if self.n_waiting_nonzero then (
    Mutex.lock self.mutex;
    Condition.signal self.cond;
    Mutex.unlock self.mutex
  )

(** Push into worker's local queue, open to work stealing.
    precondition: this runs on the worker thread whose state is [self] *)
let schedule_on_current_worker (self : worker_state) task : unit =
  (* we're on this same pool, schedule in the worker's state. Otherwise
     we might also be on pool A but asking to schedule on pool B,
     so we have to check that identifiers match. *)
  let pushed = WSQ.push self.q task in
  if pushed then
    try_wake_someone_ self.st
  else (
    (* overflow into main queue *)
    Mutex.lock self.st.mutex;
    Queue.push task self.st.main_q;
    if self.st.n_waiting_nonzero then Condition.signal self.st.cond;
    Mutex.unlock self.st.mutex
  )

(** Push into the shared queue of this pool *)
let schedule_in_main_queue (self : state) task : unit =
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

let schedule_from_w (self : worker_state) (task : WL.task_full) : unit =
  match get_current_worker_ () with
  | Some w when Id.equal self.st.id_ w.st.id_ ->
    (* use worker from the same pool *)
    schedule_on_current_worker w task
  | _ -> schedule_in_main_queue self.st task

exception Got_task of WL.task_full

(** Try to steal a task.
    @raise Got_task if it finds one. *)
let try_to_steal_work_once_ (self : worker_state) : unit =
  let init = Random.State.int self.rng (Array.length self.st.workers) in
  for i = 0 to Array.length self.st.workers - 1 do
    let w' =
      Array.unsafe_get self.st.workers
        ((i + init) mod Array.length self.st.workers)
    in

    if self != w' then (
      match WSQ.steal w'.q with
      | Some t -> raise_notrace (Got_task t)
      | None -> ()
    )
  done

(** Wait on condition. Precondition: we hold the mutex. *)
let[@inline] wait_for_condition_ (self : state) : unit =
  self.n_waiting <- self.n_waiting + 1;
  if self.n_waiting = 1 then self.n_waiting_nonzero <- true;
  Condition.wait self.cond self.mutex;
  self.n_waiting <- self.n_waiting - 1;
  if self.n_waiting = 0 then self.n_waiting_nonzero <- false

let rec get_next_task (self : worker_state) : WL.task_full =
  (* see if we can empty the local queue *)
  match WSQ.pop_exn self.q with
  | task ->
    try_wake_someone_ self.st;
    task
  | exception WSQ.Empty -> try_to_steal_from_other_workers_ self

and try_to_steal_from_other_workers_ (self : worker_state) =
  match try_to_steal_work_once_ self with
  | exception Got_task task -> task
  | () -> wait_on_main_queue self

and wait_on_main_queue (self : worker_state) : WL.task_full =
  Mutex.lock self.st.mutex;
  match Queue.pop self.st.main_q with
  | task ->
    Mutex.unlock self.st.mutex;
    task
  | exception Queue.Empty ->
    (* wait here *)
    if A.get self.st.active then (
      wait_for_condition_ self.st;

      (* see if a task became available *)
      match Queue.pop self.st.main_q with
      | task ->
        Mutex.unlock self.st.mutex;
        task
      | exception Queue.Empty ->
        Mutex.unlock self.st.mutex;
        try_to_steal_from_other_workers_ self
    ) else (
      (* do nothing more: no task in main queue, and we are shutting
         down so no new task should arrive.
         The exception is if another task is creating subtasks
         that overflow into the main queue, but we can ignore that at
         the price of slightly decreased performance for the last few
         tasks *)
      Mutex.unlock self.st.mutex;
      raise WL.No_more_tasks
    )

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
  let around_task st = st.st.around_task in
  let on_exn (st : worker_state) (ebt : Exn_bt.t) =
    st.st.on_exn (Exn_bt.exn ebt) (Exn_bt.bt ebt)
  in
  {
    WL.schedule = schedule_from_w;
    runner;
    get_next_task;
    get_thread_state = get_current_worker_exn;
    around_task;
    on_exn;
    before_start;
    cleanup;
  }

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let shutdown_ ~wait (self : state) : unit =
  if A.exchange self.active false then (
    Mutex.lock self.mutex;
    Condition.broadcast self.cond;
    Mutex.unlock self.mutex;
    if wait then Array.iter (fun w -> Thread.join w.thread) self.workers
  )

let as_runner_ (self : state) : t =
  Runner.For_runner_implementors.create
    ~shutdown:(fun ~wait () -> shutdown_ self ~wait)
    ~run_async:(fun ~fiber f ->
      schedule_in_main_queue self @@ T_start { fiber; f })
    ~size:(fun () -> size_ self)
    ~num_tasks:(fun () -> num_tasks_ self)
    ()

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?num_threads:int ->
  ?name:string ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
    ?around_task ?num_threads ?name () : t =
  let pool_id_ = Id.create () in
  (* wrapper *)
  let around_task =
    match around_task with
    | Some (f, g) -> WL.AT_pair (f, g)
    | None -> WL.AT_pair (ignore, fun _ _ -> ())
  in

  let num_domains = Domain_pool_.max_number_of_domains () in
  let num_threads = Util_pool_.num_threads ?num_threads () in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int num_domains in

  let pool =
    {
      id_ = pool_id_;
      active = A.make true;
      workers = [||];
      main_q = Queue.create ();
      n_waiting = 0;
      n_waiting_nonzero = true;
      mutex = Mutex.create ();
      cond = Condition.create ();
      around_task;
      on_exn;
      on_init_thread;
      on_exit_thread;
      name;
      as_runner = Runner.dummy;
    }
  in
  pool.as_runner <- as_runner_ pool;

  (* temporary queue used to obtain thread handles from domains
     on which the thread are started. *)
  let receive_threads = Bb_queue.create () in

  (* start the thread with index [i] *)
  let create_worker_state idx =
    let dom_id = (offset + idx) mod num_domains in
    {
      st = pool;
      thread = (* dummy *) Thread.self ();
      q = WSQ.create ~dummy:WL._dummy_task ();
      rng = Random.State.make [| idx |];
      dom_id;
      idx;
    }
  in

  pool.workers <- Array.init num_threads create_worker_state;

  (* start the thread with index [i] *)
  let start_thread_with_idx idx (st : worker_state) =
    (* function called in domain with index [i], to
       create the thread and push it into [receive_threads] *)
    let create_thread_in_domain () =
      let thread = Thread.create (WL.worker_loop ~ops:worker_ops) st in
      (* send the thread from the domain back to us *)
      Bb_queue.push receive_threads (idx, thread)
    in
    Domain_pool_.run_on st.dom_id create_thread_in_domain
  in

  (* start all worker threads, placing them on the domains
     according to their index and [offset] in a round-robin fashion. *)
  Array.iteri start_thread_with_idx pool.workers;

  (* receive the newly created threads back from domains *)
  for _j = 1 to num_threads do
    let i, th = Bb_queue.pop receive_threads in
    let worker_state = pool.workers.(i) in
    worker_state.thread <- th
  done;

  pool.as_runner

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?num_threads
    ?name () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?num_threads
      ?name ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
