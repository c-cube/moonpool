open Types_
include Runner
module WL = Worker_loop_

type fiber = Picos.Fiber.t
type task_full = WL.task_full

let ( let@ ) = ( @@ )

type state = {
  threads: Thread.t array;
  q: task_full Bb_queue.t;  (** Queue for tasks. *)
  mutable as_runner: t;
  (* init options *)
  name: string option;
  on_init_thread: dom_id:int -> t_id:int -> unit -> unit;
  on_exit_thread: dom_id:int -> t_id:int -> unit -> unit;
  on_exn: exn -> Printexc.raw_backtrace -> unit;
}
(** internal state *)

type worker_state = {
  idx: int;
  dom_idx: int;
  st: state;
}

let[@inline] size_ (self : state) = Array.length self.threads
let[@inline] num_tasks_ (self : state) : int = Bb_queue.size self.q

(*
get_thread_state = TLS.get_opt k_worker_state
  *)

let default_thread_init_exit_ ~dom_id:_ ~t_id:_ () = ()

let shutdown_ ~wait (self : state) : unit =
  Bb_queue.close self.q;
  if wait then Array.iter Thread.join self.threads

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?num_threads:int ->
  ?name:string ->
  'a

(** Run [task] as is, on the pool. *)
let schedule_ (self : state) (task : task_full) : unit =
  try Bb_queue.push self.q task with Bb_queue.Closed -> raise Shutdown

let runner_of_state (pool : state) : t =
  let run_async ~fiber f = schedule_ pool @@ T_start { f; fiber } in
  Runner.For_runner_implementors.create
    ~shutdown:(fun ~wait () -> shutdown_ pool ~wait)
    ~run_async
    ~size:(fun () -> size_ pool)
    ~num_tasks:(fun () -> num_tasks_ pool)
    ()

(** Run [task] as is, on the pool. *)
let schedule_w (self : worker_state) (task : task_full) : unit =
  try Bb_queue.push self.st.q task with Bb_queue.Closed -> raise Shutdown

let get_next_task (self : worker_state) =
  try Bb_queue.pop self.st.q with Bb_queue.Closed -> raise WL.No_more_tasks

let before_start (self : worker_state) =
  let t_id = Thread.id @@ Thread.self () in
  self.st.on_init_thread ~dom_id:self.dom_idx ~t_id ();

  (* set thread name *)
  Option.iter
    (fun name ->
      Tracing_.set_thread_name (Printf.sprintf "%s.worker.%d" name self.idx))
    self.st.name

let cleanup (self : worker_state) : unit =
  (* on termination, decrease refcount of underlying domain *)
  Domain_pool_.decr_on self.dom_idx;
  let t_id = Thread.id @@ Thread.self () in
  self.st.on_exit_thread ~dom_id:self.dom_idx ~t_id ()

let worker_ops : worker_state WL.ops =
  let runner (st : worker_state) = st.st.as_runner in
  let on_exn (st : worker_state) (ebt : Exn_bt.t) =
    st.st.on_exn (Exn_bt.exn ebt) (Exn_bt.bt ebt)
  in
  {
    WL.schedule = schedule_w;
    runner;
    get_next_task;
    on_exn;
    before_start;
    cleanup;
  }

let create_ ?(on_init_thread = default_thread_init_exit_)
    ?(on_exit_thread = default_thread_init_exit_) ?(on_exn = fun _ _ -> ())
    ~threads ?name () : state =
  let self =
    {
      threads;
      q = Bb_queue.create ();
      as_runner = Runner.dummy;
      name;
      on_init_thread;
      on_exit_thread;
      on_exn;
    }
  in
  self.as_runner <- runner_of_state self;
  self

let create ?on_init_thread ?on_exit_thread ?on_exn ?num_threads ?name () : t =
  let num_domains = Domain_pool_.max_number_of_domains () in

  (* number of threads to run *)
  let num_threads = Util_pool_.num_threads ?num_threads () in

  (* make sure we don't bias towards the first domain(s) in {!D_pool_} *)
  let offset = Random.int num_domains in

  let pool =
    let dummy_thread = Thread.self () in
    let threads = Array.make num_threads dummy_thread in
    create_ ?on_init_thread ?on_exit_thread ?on_exn ~threads ?name ()
  in
  let runner = runner_of_state pool in

  (* temporary queue used to obtain thread handles from domains
     on which the thread are started. *)
  let receive_threads = Bb_queue.create () in

  (* start the thread with index [i] *)
  let start_thread_with_idx i =
    let dom_idx = (offset + i) mod num_domains in

    (* function called in domain with index [i], to
       create the thread and push it into [receive_threads] *)
    let create_thread_in_domain () =
      let st = { idx = i; dom_idx; st = pool } in
      let thread =
        Thread.create (WL.worker_loop ~block_signals:true ~ops:worker_ops) st
      in
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

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?num_threads ?name () f =
  let pool =
    create ?on_init_thread ?on_exit_thread ?on_exn ?num_threads ?name ()
  in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool

module Private_ = struct
  type nonrec worker_state = worker_state

  let worker_ops = worker_ops
  let runner_of_state (self : worker_state) = worker_ops.runner self

  let create_single_threaded_state ~thread ?on_exn () : worker_state =
    let st : state = create_ ?on_exn ~threads:[| thread |] () in
    { idx = 0; dom_idx = 0; st }
end
