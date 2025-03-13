open Types_

type fiber = Picos.Fiber.t

type task_full =
  | T_start of {
      fiber: fiber;
      f: unit -> unit;
    }
  | T_resume : {
      fiber: fiber;
      k: unit -> unit;
    }
      -> task_full

type around_task =
  | AT_pair : (Runner.t -> 'a) * (Runner.t -> 'a -> unit) -> around_task

exception No_more_tasks

type 'st ops = {
  schedule: 'st -> task_full -> unit;
  get_next_task: 'st -> task_full;  (** @raise No_more_tasks *)
  get_thread_state: unit -> 'st;
      (** Access current thread's worker state from any worker *)
  around_task: 'st -> around_task;
  on_exn: 'st -> Exn_bt.t -> unit;
  runner: 'st -> Runner.t;
  before_start: 'st -> unit;
  cleanup: 'st -> unit;
}

(** A dummy task. *)
let _dummy_task : task_full = T_start { f = ignore; fiber = _dummy_fiber }

[@@@ifge 5.0]

let[@inline] discontinue k exn =
  let bt = Printexc.get_raw_backtrace () in
  Effect.Deep.discontinue_with_backtrace k exn bt

let with_handler (type st arg) ~(ops : st ops) (self : st) :
    (unit -> unit) -> unit =
  let current =
    Some
      (fun k ->
        match get_current_fiber_exn () with
        | fiber -> Effect.Deep.continue k fiber
        | exception exn -> discontinue k exn)
  and yield =
    Some
      (fun k ->
        let fiber = get_current_fiber_exn () in
        match
          let k () = Effect.Deep.continue k () in
          ops.schedule self @@ T_resume { fiber; k }
        with
        | () -> ()
        | exception exn -> discontinue k exn)
  and reschedule trigger fiber k : unit =
    ignore (Picos.Fiber.unsuspend fiber trigger : bool);
    let k () = Picos.Fiber.resume fiber k in
    let task = T_resume { fiber; k } in
    ops.schedule self task
  in
  let effc (type a) :
      a Effect.t -> ((a, _) Effect.Deep.continuation -> _) option = function
    | Picos.Fiber.Current -> current
    | Picos.Fiber.Yield -> yield
    | Picos.Fiber.Spawn r ->
      Some
        (fun k ->
          match
            let f () = r.main r.fiber in
            let task = T_start { fiber = r.fiber; f } in
            ops.schedule self task
          with
          | unit -> Effect.Deep.continue k unit
          | exception exn -> discontinue k exn)
    | Picos.Trigger.Await trigger ->
      Some
        (fun k ->
          let fiber = get_current_fiber_exn () in
          (* when triggers is signaled, reschedule task *)
          if not (Picos.Fiber.try_suspend fiber trigger fiber k reschedule) then
            (* trigger was already signaled, run task now *)
            Picos.Fiber.resume fiber k)
    | Picos.Computation.Cancel_after _r ->
      Some
        (fun k ->
          (* not implemented *)
          let exn = Failure "Moonpool: cancel_after is not supported." in
          discontinue k exn)
    | _ -> None
  in
  let handler = Effect.Deep.{ retc = Fun.id; exnc = raise; effc } in
  fun f -> Effect.Deep.match_with f () handler

[@@@else_]

let with_handler ~ops:_ self f = f ()

[@@@endif]

let worker_loop (type st) ~block_signals ~(ops : st ops) (self : st) : unit =
  if block_signals then (
    try
      ignore
        (Unix.sigprocmask SIG_BLOCK
           [
             Sys.sigterm;
             Sys.sigpipe;
             Sys.sigint;
             Sys.sigchld;
             Sys.sigalrm;
             Sys.sigusr1;
             Sys.sigusr2;
           ]
          : _ list)
    with _ -> ()
  );

  let cur_fiber : fiber ref = ref _dummy_fiber in
  let runner = ops.runner self in
  TLS.set Runner.For_runner_implementors.k_cur_runner runner;

  let (AT_pair (before_task, after_task)) = ops.around_task self in

  let run_task (task : task_full) : unit =
    let fiber =
      match task with
      | T_start { fiber; _ } | T_resume { fiber; _ } -> fiber
    in

    cur_fiber := fiber;
    TLS.set k_cur_fiber fiber;
    let _ctx = before_task runner in

    (* run the task now, catching errors, handling effects *)
    assert (task != _dummy_task);
    (try
       match task with
       | T_start { fiber = _; f } -> with_handler ~ops self f
       | T_resume { fiber = _; k } ->
         (* this is already in an effect handler *)
         k ()
     with e ->
       let ebt = Exn_bt.get e in
       ops.on_exn self ebt);

    after_task runner _ctx;

    cur_fiber := _dummy_fiber;
    TLS.set k_cur_fiber _dummy_fiber
  in

  ops.before_start self;

  let continue = ref true in
  try
    while !continue do
      match ops.get_next_task self with
      | task -> run_task task
      | exception No_more_tasks -> continue := false
    done;
    ops.cleanup self
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    ops.cleanup self;
    Printexc.raise_with_backtrace exn bt
