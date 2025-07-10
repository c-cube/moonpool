module Exn_bt = Moonpool.Exn_bt

open struct
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
    tasks: WL.task_full Queue.t;
    actions_from_other_threads: (unit -> unit) Queue.t;
        (** Other threads ask us to run closures in the lwt thread *)
    mutex: Mutex.t;
    mutable thread: int;
    mutable closed: bool;
    mutable as_runner: Moonpool.Runner.t;
    mutable enter_hook: Lwt_main.Enter_iter_hooks.hook option;
    mutable leave_hook: Lwt_main.Leave_iter_hooks.hook option;
    mutable notification: int;
        (** A lwt_unix notification to wake up the event loop *)
    has_notified: bool Atomic.t;
  }

  let st : st =
    {
      tasks = Queue.create ();
      actions_from_other_threads = Queue.create ();
      mutex = Mutex.create ();
      thread = Thread.self () |> Thread.id;
      closed = false;
      as_runner = Moonpool.Runner.dummy;
      enter_hook = None;
      leave_hook = None;
      notification = 0;
      has_notified = Atomic.make false;
    }
end

module Ops = struct
  type st = Scheduler_state.st

  let around_task _ = default_around_task_

  let add_action_from_another_thread_ (self : st) f : unit =
    Mutex.lock Scheduler_state.st.mutex;
    Queue.push f self.actions_from_other_threads;
    Mutex.unlock Scheduler_state.st.mutex;
    if not (Atomic.exchange self.has_notified true) then
      Lwt_unix.send_notification self.notification

  let schedule (self : st) t =
    if Thread.id (Thread.self ()) = self.thread then
      Queue.push t self.tasks
    else
      add_action_from_another_thread_ self (fun () -> Queue.push t self.tasks)

  let get_next_task (self : st) =
    if self.closed then raise WL.No_more_tasks;
    try Queue.pop self.tasks with Queue.Empty -> raise WL.No_more_tasks

  let on_exn _ ebt = !on_uncaught_exn ebt
  let runner (self : st) = self.as_runner

  let as_runner (self : st) : Moonpool.Runner.t =
    Moonpool.Runner.For_runner_implementors.create
      ~size:(fun () -> 1)
      ~num_tasks:(fun () ->
        (* FIXME: thread safety. use an atomic?? *)
        Queue.length self.tasks)
      ~run_async:(fun ~fiber f -> schedule self @@ WL.T_start { fiber; f })
      ~shutdown:(fun ~wait:_ () -> self.closed <- true)
      ()

  let before_start (self : st) : unit =
    self.as_runner <- as_runner self;
    ()

  let cleanup (self : st) =
    self.closed <- true;
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
  module FG =
    WL.Fine_grained
      (struct
        include Scheduler_state

        let ops = Ops.ops
      end)
      ()
end

let await_lwt (fut : _ Lwt.t) =
  match Lwt.state fut with
  | Return x -> x
  | Fail exn -> raise exn
  | Sleep ->
    (* suspend fiber, wake it up when [fut] resolves *)
    let trigger = M.Trigger.create () in
    Lwt.on_termination fut (fun _ -> M.Trigger.signal trigger);
    M.Trigger.await trigger |> Option.iter Exn_bt.raise;

    (match Lwt.state fut with
    | Return x -> x
    | Fail exn -> raise exn
    | Sleep -> assert false)

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
  (* execute actions sent from other threads *)
  let local_acts = Queue.create () in
  Mutex.lock Scheduler_state.st.mutex;
  Queue.transfer Scheduler_state.st.actions_from_other_threads local_acts;
  Mutex.unlock Scheduler_state.st.mutex;
  Queue.iter (fun f -> f ()) local_acts;

  (* run tasks *)
  FG.run ~max_tasks:1000 ();

  if not (Queue.is_empty Scheduler_state.st.tasks) then
    ignore (Lwt.pause () : unit Lwt.t);
  ()

let is_setup_ = ref false

let setup () =
  if not !is_setup_ then (
    is_setup_ := true;
    FG.setup ~block_signals:false ();
    Scheduler_state.st.enter_hook <-
      Some (Lwt_main.Enter_iter_hooks.add_last run_in_hook);
    Scheduler_state.st.leave_hook <-
      Some (Lwt_main.Leave_iter_hooks.add_last run_in_hook);
    Scheduler_state.st.notification <-
      Lwt_unix.make_notification ~once:false run_in_hook
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
  Scheduler_state.st.thread <- Thread.self () |> Thread.id;
  let fut = spawn_lwt (fun () -> f Scheduler_state.st.as_runner) in
  Lwt_main.run fut
