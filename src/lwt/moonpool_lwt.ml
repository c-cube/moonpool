open Common_

open struct
  module BQ = Moonpool.Blocking_queue
  module WL = Moonpool.Private.Worker_loop_
  module M = Moonpool
end

module Fut = Moonpool.Fut

let default_around_task_ : WL.around_task = AT_pair (ignore, fun _ _ -> ())

let on_uncaught_exn : (Moonpool.Exn_bt.t -> unit) ref =
  ref (fun ebt ->
      Printf.eprintf "uncaught exception in moonpool-lwt:\n%s" (Exn_bt.show ebt))

module Scheduler_state = struct
  type st = {
    tasks: WL.task_full BQ.t;
    mutable as_runner: Moonpool.Runner.t;
    mutable enter_hook: Lwt_main.Enter_iter_hooks.hook option;
    mutable leave_hook: Lwt_main.Leave_iter_hooks.hook option;
  }

  let st : st =
    {
      tasks = BQ.create ();
      as_runner = Moonpool.Runner.dummy;
      enter_hook = None;
      leave_hook = None;
    }

  let around_task _ = default_around_task_

  (* FIXME: need to wakeup lwt if needed! *)
  let schedule (self : st) t = BQ.push self.tasks t

  let get_next_task (self : st) =
    try BQ.pop self.tasks with BQ.Closed -> raise WL.No_more_tasks

  let on_exn _ ebt = !on_uncaught_exn ebt
  let runner self = self.as_runner

  let as_runner (self : st) : Moonpool.Runner.t =
    Moonpool.Runner.For_runner_implementors.create
      ~size:(fun () -> 1)
      ~num_tasks:(fun () -> BQ.size self.tasks)
      ~run_async:(fun ~fiber f -> schedule self @@ WL.T_start { fiber; f })
      ~shutdown:(fun ~wait:_ () -> BQ.close self.tasks)
      ()

  let before_start self : unit =
    self.as_runner <- as_runner self;
    ()

  let cleanup self =
    Option.iter Lwt_main.Enter_iter_hooks.remove self.enter_hook;
    Option.iter Lwt_main.Leave_iter_hooks.remove self.leave_hook;
    ()

  let ops : st WL.ops =
    {
      schedule;
      around_task;
      get_next_task;
      on_exn;
      runner;
      before_start;
      cleanup;
    }
end

open struct
  module FG = WL.Fine_grained (Scheduler_state) ()
end

let _dummy_exn_bt : Exn_bt.t =
  Exn_bt.get_callstack 0 (Failure "dummy Exn_bt from moonpool-lwt")

let await_lwt (fut : _ Lwt.t) =
  match Lwt.poll fut with
  | Some x -> x
  | None ->
    (* suspend fiber, wake it up when [fut] resolves *)
    let trigger = M.Trigger.create () in
    let res = ref (Error _dummy_exn_bt) in
    Lwt.on_termination fut (fun _ -> M.Trigger.signal trigger);
    M.Trigger.await trigger |> Option.iter Exn_bt.raise;
    Exn_bt.unwrap !res

let lwt_of_fut (fut : 'a M.Fut.t) : 'a Lwt.t =
  let lwt_fut, lwt_prom = Lwt.wait () in
  M.Fut.on_result fut (function
    | Ok x -> Lwt.wakeup lwt_prom x
    | Error ebt ->
      let exn = Exn_bt.exn ebt in
      Lwt.wakeup_exn lwt_prom exn);
  lwt_fut

let fut_of_lwt (lwt_fut : _ Lwt.t) : _ M.Fut.t =
  match Lwt.poll lwt_fut with
  | Some x -> M.Fut.return x
  | None ->
    let fut, prom = M.Fut.make () in
    Lwt.on_any lwt_fut
      (fun x -> M.Fut.fulfill prom (Ok x))
      (fun exn ->
        let bt = Printexc.get_callstack 10 in
        M.Fut.fulfill prom (Error (Exn_bt.make exn bt)));
    fut

let run_in_hook () =
  FG.run ~max_tasks:1000 ();
  if BQ.size Scheduler_state.st.tasks > 0 then
    ignore (Lwt.pause () : unit Lwt.t)

let is_setup_ = ref false

let setup () =
  if not !is_setup_ then (
    is_setup_ := true;
    FG.setup ~block_signals:false ();
    Scheduler_state.st.enter_hook <-
      Some (Lwt_main.Enter_iter_hooks.add_last run_in_hook);
    Scheduler_state.st.leave_hook <-
      Some (Lwt_main.Leave_iter_hooks.add_last run_in_hook)
  )

let spawn_lwt f : _ Lwt.t =
  setup ();
  let lwt_fut, lwt_prom = Lwt.wait () in
  M.Runner.run_async Scheduler_state.st.as_runner (fun () ->
      try
        let x = f () in
        Lwt.wakeup lwt_prom x
      with exn -> Lwt.wakeup_exn lwt_prom exn);
  lwt_fut

let lwt_main (f : _ -> 'a) : 'a =
  setup ();
  let fut = spawn_lwt (fun () -> f Scheduler_state.st.as_runner) in
  Lwt_main.run fut
