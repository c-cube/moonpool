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

exception No_more_tasks

type 'st ops = {
  schedule: 'st -> task_full -> unit;
  get_next_task: 'st -> task_full;  (** @raise No_more_tasks *)
  on_exn: 'st -> Exn_bt.t -> unit;
  runner: 'st -> Runner.t;
  before_start: 'st -> unit;
  cleanup: 'st -> unit;
}

(** A dummy task. *)
let _dummy_task : task_full = T_start { f = ignore; fiber = _dummy_fiber }

let[@inline] discontinue k exn =
  let bt = Printexc.get_raw_backtrace () in
  Effect.Deep.discontinue_with_backtrace k exn bt

let[@inline] raise_with_bt exn =
  let bt = Printexc.get_raw_backtrace () in
  Printexc.raise_with_backtrace exn bt

let with_handler (type st) ~(ops : st ops) (self : st) : (unit -> unit) -> unit
    =
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
            (* trigger was already signaled, reschedule task now *)
            reschedule trigger fiber k)
    | Picos.Computation.Cancel_after _r ->
      Some
        (fun k ->
          (* not implemented *)
          let exn = Failure "Moonpool: cancel_after is not supported." in
          discontinue k exn)
    | _ -> None
  in
  let handler = Effect.Deep.{ retc = Fun.id; exnc = raise_with_bt; effc } in
  fun f -> Effect.Deep.match_with f () handler

module type FINE_GRAINED_ARGS = sig
  type st

  val ops : st ops
  val st : st
end

module Fine_grained (Args : FINE_GRAINED_ARGS) () = struct
  open Args

  let cur_st : Runner.For_runner_implementors.thread_local_state Lazy.t =
    lazy
      (match TLS.get_exn Runner.For_runner_implementors.k_cur_st with
      | st -> st
      | exception TLS.Not_set ->
        failwith "Moonpool: worker loop: no current state set")

  let runner = ops.runner st

  type state =
    | New
    | Ready
    | Torn_down

  let state = ref New

  let run_task (task : task_full) : unit =
    let fiber =
      match task with
      | T_start { fiber; _ } | T_resume { fiber; _ } -> fiber
    in

    (Lazy.force cur_st).cur_fiber <- fiber;

    (* run the task now, catching errors, handling effects *)
    assert (task != _dummy_task);
    (try
       match task with
       | T_start { fiber = _; f } -> with_handler ~ops st f
       | T_resume { fiber = _; k } ->
         (* this is already in an effect handler *)
         k ()
     with e ->
       let bt = Printexc.get_raw_backtrace () in
       let ebt = Exn_bt.make e bt in
       ops.on_exn st ebt);

    (Lazy.force cur_st).cur_fiber <- _dummy_fiber

  let setup ~block_signals () : unit =
    if !state <> New then invalid_arg "worker_loop.setup: not a new instance";
    state := Ready;

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

    ops.before_start st;
    (Lazy.force cur_st).runner <- runner;
    ()

  let run ?(max_tasks = max_int) () : unit =
    if !state <> Ready then invalid_arg "worker_loop.run: not setup";

    let continue = ref true in
    let n_tasks = ref 0 in
    while !continue && !n_tasks < max_tasks do
      match ops.get_next_task st with
      | task ->
        incr n_tasks;
        run_task task
      | exception No_more_tasks -> continue := false
    done

  let teardown () =
    if !state <> Torn_down then (
      state := Torn_down;
      (Lazy.force cur_st).cur_fiber <- _dummy_fiber;
      ops.cleanup st
    )
end

let worker_loop (type st) ~block_signals ~(ops : st ops) (self : st) : unit =
  let module FG =
    Fine_grained
      (struct
        type nonrec st = st

        let ops = ops
        let st = self
      end)
      ()
  in
  FG.setup ~block_signals ();
  try
    FG.run ();
    FG.teardown ()
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    FG.teardown ();
    Printexc.raise_with_backtrace exn bt
