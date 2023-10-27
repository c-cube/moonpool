module WSQ = Ws_deque_
module A = Atomic_
module TLS = Thread_local_storage
include Runner

let ( let@ ) = ( @@ )

type worker_state = {
  mutable thread: Thread.t;
  q: task WSQ.t;  (** Work stealing queue *)
  mutable work_steal_offset: int;  (** Current offset for work stealing *)
}
(** State for a given worker. Only this worker is
    allowed to push into the queue, but other workers
    can come and steal from it if they're idle. *)

type around_task = AT_pair : (t -> 'a) * (t -> 'a -> unit) -> around_task

type state = {
  active: bool A.t;  (** Becomes [false] when the pool is shutdown. *)
  workers: worker_state array;  (** Fixed set of workers. *)
  main_q: task Queue.t;  (** Main queue for tasks coming from the outside *)
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
let schedule_task_ (self : state) (w : worker_state option) (task : task) : unit
    =
  (* Printf.printf "schedule task now (%d)\n%!" (Thread.id @@ Thread.self ()); *)
  match w with
  | Some w ->
    WSQ.push w.q task;
    try_wake_someone_ self
  | None ->
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
let run_task_now_ (self : state) ~runner task : unit =
  (* Printf.printf "run task now (%d)\n%!" (Thread.id @@ Thread.self ()); *)
  let (AT_pair (before_task, after_task)) = self.around_task in
  let _ctx = before_task runner in
  (* run the task now, catching errors *)
  (try
     (* run [task()] and handle [suspend] in it *)
     Suspend_.with_suspend task ~run:(fun task' ->
         let w = find_current_worker_ () in
         schedule_task_ self w task')
   with e ->
     let bt = Printexc.get_raw_backtrace () in
     self.on_exn e bt);
  after_task runner _ctx

let[@inline] run_async_ (self : state) (task : task) : unit =
  let w = find_current_worker_ () in
  schedule_task_ self w task

(* TODO: function to schedule many tasks from the outside.
    - build a queue
    - lock
    - queue transfer
    - wakeup all (broadcast)
    - unlock *)

let run = run_async

(** Wait on condition. Precondition: we hold the mutex. *)
let[@inline] wait_ (self : state) : unit =
  self.n_waiting <- self.n_waiting + 1;
  if self.n_waiting = 1 then self.n_waiting_nonzero <- true;
  Condition.wait self.cond self.mutex;
  self.n_waiting <- self.n_waiting - 1;
  if self.n_waiting = 0 then self.n_waiting_nonzero <- false

(** Try to steal a task from the worker [w] *)
let try_to_steal_work_once_ (self : state) (w : worker_state) : task option =
  w.work_steal_offset <- (w.work_steal_offset + 1) mod Array.length self.workers;

  (* if we're pointing to [w], skip to the next worker as
     it's useless to steal from oneself *)
  if Array.unsafe_get self.workers w.work_steal_offset == w then
    w.work_steal_offset <-
      (w.work_steal_offset + 1) mod Array.length self.workers;

  let w' = Array.unsafe_get self.workers w.work_steal_offset in
  WSQ.steal w'.q

(** Try to steal work from several other workers. *)
let try_to_steal_work_loop (self : state) ~runner w : bool =
  if size_ self = 1 then
    (* no stealing for single thread pool *)
    false
  else (
    let has_stolen = ref false in
    let n_retries_left = ref (size_ self - 1) in

    while !n_retries_left > 0 do
      match try_to_steal_work_once_ self w with
      | Some task ->
        try_wake_someone_ self;
        run_task_now_ self ~runner task;
        has_stolen := true;
        n_retries_left := 0
      | None -> decr n_retries_left
    done;
    !has_stolen
  )

(** Worker runs tasks from its queue until none remains *)
let worker_run_self_tasks_ (self : state) ~runner w : unit =
  let continue = ref true in
  while !continue && A.get self.active do
    match WSQ.pop w.q with
    | Some task ->
      try_wake_someone_ self;
      run_task_now_ self ~runner task
    | None -> continue := false
  done

(** Main loop for a worker thread. *)
let worker_thread_ (self : state) ~(runner : t) (w : worker_state) : unit =
  TLS.get k_worker_state := Some w;

  let main_loop () : unit =
    let continue = ref true in
    while !continue && A.get self.active do
      worker_run_self_tasks_ self ~runner w;

      let did_steal = try_to_steal_work_loop self ~runner w in
      if not did_steal then (
        Mutex.lock self.mutex;
        match Queue.pop self.main_q with
        | task ->
          Mutex.unlock self.mutex;
          run_task_now_ self ~runner task
        | exception Queue.Empty ->
          if A.get self.active then wait_ self;
          Mutex.unlock self.mutex
      )
    done;
    assert (WSQ.size w.q = 0)
  in

  (* handle domain-local await *)
  Dla_.using ~prepare_for_await:Suspend_.prepare_for_await
    ~while_running:main_loop

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
  ?min:int ->
  ?per_domain:int ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

let create ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
    ?around_task ?min:(min_threads = 1) ?(per_domain = 0) () : t =
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
    Array.init num_threads (fun i ->
        {
          thread = dummy;
          q = WSQ.create ();
          work_steal_offset = (i + 1) mod num_threads;
        })
  in

  let pool =
    {
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

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?min ?per_domain
    () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?around_task ?min ?per_domain
      ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
