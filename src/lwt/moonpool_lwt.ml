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
    closed: bool Atomic.t;
    cleanup_done: bool Atomic.t;
    mutable as_runner: Moonpool.Runner.t;
    mutable enter_hook: Lwt_main.Enter_iter_hooks.hook option;
    mutable leave_hook: Lwt_main.Leave_iter_hooks.hook option;
    mutable notification: int;
        (** A lwt_unix notification to wake up the event loop *)
    has_notified: bool Atomic.t;
  }

  (** Main state *)
  let cur_st : st option Atomic.t = Atomic.make None

  let create_new () : st =
    {
      tasks = Queue.create ();
      actions_from_other_threads = Queue.create ();
      mutex = Mutex.create ();
      thread = Thread.id (Thread.self ());
      closed = Atomic.make false;
      cleanup_done = Atomic.make false;
      as_runner = Moonpool.Runner.dummy;
      enter_hook = None;
      leave_hook = None;
      notification = 0;
      has_notified = Atomic.make false;
    }

  let[@inline] notify_ (self : st) : unit =
    if not (Atomic.exchange self.has_notified true) then
      Lwt_unix.send_notification self.notification

  let[@inline never] add_action_from_another_thread_ (self : st) f : unit =
    Mutex.lock self.mutex;
    Queue.push f self.actions_from_other_threads;
    Mutex.unlock self.mutex;
    notify_ self

  let[@inline] on_lwt_thread_ (self : st) : bool =
    Thread.id (Thread.self ()) = self.thread

  let[@inline] run_on_lwt_thread_ (self : st) (f : unit -> unit) : unit =
    if on_lwt_thread_ self then
      f ()
    else
      add_action_from_another_thread_ self f

  let cleanup (st : st) =
    match Atomic.get cur_st with
    | Some st' ->
      if st != st' then
        failwith
          "moonpool-lwt: cleanup failed (state is not the currently active \
           one!)";
      if not (on_lwt_thread_ st) then
        failwith "moonpool-lwt: cleanup from the wrong thread";
      Atomic.set st.closed true;
      if not (Atomic.exchange st.cleanup_done true) then (
        Option.iter Lwt_main.Enter_iter_hooks.remove st.enter_hook;
        Option.iter Lwt_main.Leave_iter_hooks.remove st.leave_hook;
        Lwt_unix.stop_notification st.notification
      );

      Atomic.set cur_st None
    | None -> failwith "moonpool-lwt: cleanup failed (no current active state)"
end

module Ops = struct
  type st = Scheduler_state.st

  let around_task _ = default_around_task_

  let schedule (self : st) t =
    if Atomic.get self.closed then
      failwith "moonpool-lwt.schedule: scheduler is closed";
    Scheduler_state.run_on_lwt_thread_ self (fun () -> Queue.push t self.tasks)

  let get_next_task (self : st) =
    if Atomic.get self.closed then raise WL.No_more_tasks;
    try Queue.pop self.tasks with Queue.Empty -> raise WL.No_more_tasks

  let on_exn _ ebt = !on_uncaught_exn ebt
  let runner (self : st) = self.as_runner
  let cleanup = Scheduler_state.cleanup

  let as_runner (self : st) : Moonpool.Runner.t =
    Moonpool.Runner.For_runner_implementors.create
      ~size:(fun () -> 1)
      ~num_tasks:(fun () ->
        Mutex.lock self.mutex;
        let n = Queue.length self.tasks in
        Mutex.unlock self.mutex;
        n)
      ~run_async:(fun ~fiber f -> schedule self @@ WL.T_start { fiber; f })
      ~shutdown:(fun ~wait:_ () -> Atomic.set self.closed true)
      ()

  let before_start (self : st) : unit =
    self.as_runner <- as_runner self;
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

  let setup st =
    if Atomic.compare_and_set Scheduler_state.cur_st None (Some st) then
      before_start st
    else
      failwith "moonpool-lwt: setup failed (state already in place)"
end

(** Resolve [prom] with the result of [lwt_fut] *)
let transfer_lwt_to_fut (lwt_fut : 'a Lwt.t) (prom : 'a Fut.promise) : unit =
  Lwt.on_any lwt_fut
    (fun x -> M.Fut.fulfill prom (Ok x))
    (fun exn ->
      let bt = Printexc.get_callstack 10 in
      M.Fut.fulfill prom (Error (Exn_bt.make exn bt)))

let[@inline] register_trigger_on_lwt_termination (lwt_fut : _ Lwt.t)
    (tr : M.Trigger.t) : unit =
  Lwt.on_termination lwt_fut (fun _ -> M.Trigger.signal tr)

let[@inline] await_lwt_terminated (fut : _ Lwt.t) =
  match Lwt.state fut with
  | Return x -> x
  | Fail exn -> raise exn
  | Sleep -> assert false

module Main_state = struct
  let[@inline] get_st () : Scheduler_state.st =
    match Atomic.get Scheduler_state.cur_st with
    | Some st ->
      if Atomic.get st.closed then failwith "moonpool-lwt: scheduler is closed";
      st
    | None -> failwith "moonpool-lwt: scheduler is not setup"

  let[@inline] run_on_lwt_thread f =
    Scheduler_state.run_on_lwt_thread_ (get_st ()) f

  let[@inline] on_lwt_thread () : bool =
    Scheduler_state.on_lwt_thread_ (get_st ())

  let[@inline] add_action_from_another_thread f : unit =
    Scheduler_state.add_action_from_another_thread_ (get_st ()) f
end

let await_lwt (fut : _ Lwt.t) =
  if Scheduler_state.on_lwt_thread_ (Main_state.get_st ()) then (
    (* can directly access the future *)
      match Lwt.state fut with
    | Return x -> x
    | Fail exn -> raise exn
    | Sleep ->
      let tr = M.Trigger.create () in
      register_trigger_on_lwt_termination fut tr;
      M.Trigger.await_exn tr;
      await_lwt_terminated fut
  ) else (
    let tr = M.Trigger.create () in
    Main_state.add_action_from_another_thread (fun () ->
        register_trigger_on_lwt_termination fut tr);
    M.Trigger.await_exn tr;
    await_lwt_terminated fut
  )

let lwt_of_fut (fut : 'a M.Fut.t) : 'a Lwt.t =
  if not (Main_state.on_lwt_thread ()) then
    failwith "lwt_of_fut: not on the lwt thread";
  let lwt_fut, lwt_prom = Lwt.wait () in

  (* in lwt thread, resolve [lwt_fut] *)
  let wakeup_using_res = function
    | Ok x -> Lwt.wakeup lwt_prom x
    | Error ebt ->
      let exn = Exn_bt.exn ebt in
      Lwt.wakeup_exn lwt_prom exn
  in

  M.Fut.on_result fut (fun res ->
      Main_state.run_on_lwt_thread (fun () ->
          (* can safely wakeup from the lwt thread *)
          wakeup_using_res res));

  lwt_fut

let fut_of_lwt (lwt_fut : _ Lwt.t) : _ M.Fut.t =
  if Main_state.on_lwt_thread () then (
    match Lwt.state lwt_fut with
    | Return x -> M.Fut.return x
    | _ ->
      let fut, prom = M.Fut.make () in
      transfer_lwt_to_fut lwt_fut prom;
      fut
  ) else (
    let fut, prom = M.Fut.make () in
    Main_state.add_action_from_another_thread (fun () ->
        transfer_lwt_to_fut lwt_fut prom);
    fut
  )

let run_in_lwt_and_await (f : unit -> 'a Lwt.t) : 'a =
  if Main_state.on_lwt_thread () then (
    let fut = f () in
    await_lwt fut
  ) else (
    let fut, prom = Fut.make () in
    Main_state.add_action_from_another_thread (fun () ->
        let lwt_fut = f () in
        transfer_lwt_to_fut lwt_fut prom);
    Fut.await fut
  )

module Setup_lwt_hooks (ARG : sig
  val st : Scheduler_state.st
end) =
struct
  open ARG

  module FG =
    WL.Fine_grained
      (struct
        include Scheduler_state

        let st = st
        let ops = Ops.ops
      end)
      ()

  let run_in_hook () =
    (* execute actions sent from other threads; first transfer them
     all atomically to a local queue to reduce contention *)
    let local_acts = Queue.create () in
    Mutex.lock st.mutex;
    Queue.transfer st.actions_from_other_threads local_acts;
    Atomic.set st.has_notified false;
    Mutex.unlock st.mutex;

    Queue.iter (fun f -> f ()) local_acts;

    (* run tasks *)
    FG.run ~max_tasks:1000 ();

    if not (Queue.is_empty st.tasks) then ignore (Lwt.pause () : unit Lwt.t);
    ()

  let setup () =
    (* only one thread does this *)
    FG.setup ~block_signals:false ();

    st.thread <- Thread.self () |> Thread.id;
    st.enter_hook <- Some (Lwt_main.Enter_iter_hooks.add_last run_in_hook);
    st.leave_hook <- Some (Lwt_main.Leave_iter_hooks.add_last run_in_hook);
    (* notification used to wake lwt up *)
    st.notification <- Lwt_unix.make_notification ~once:false run_in_hook
end

let setup () : Scheduler_state.st =
  let st = Scheduler_state.create_new () in
  Ops.setup st;
  let module Setup_lwt_hooks' = Setup_lwt_hooks (struct
    let st = st
  end) in
  Setup_lwt_hooks'.setup ();
  st

let[@inline] is_setup () = Option.is_some @@ Atomic.get Scheduler_state.cur_st

let spawn_lwt f : _ Lwt.t =
  let st = Main_state.get_st () in
  let lwt_fut, lwt_prom = Lwt.wait () in
  Moonpool_fib.spawn_top_ignore ~on:st.as_runner (fun () ->
      try
        let x = f () in
        Lwt.wakeup lwt_prom x
      with exn -> Lwt.wakeup_exn lwt_prom exn);
  lwt_fut

let spawn_lwt_ignore f = ignore (spawn_lwt f : unit Lwt.t)
let on_lwt_thread = Main_state.on_lwt_thread

let lwt_main (f : _ -> 'a) : 'a =
  let st = setup () in
  (* make sure to cleanup *)
  let finally () = Scheduler_state.cleanup st in
  Fun.protect ~finally @@ fun () ->
  let fut = spawn_lwt (fun () -> f st.as_runner) in
  (* make sure the scheduler isn't already sleeping *)
  Scheduler_state.notify_ st;
  Lwt_main.run fut

let[@inline] lwt_main_runner () =
  let st = Main_state.get_st () in
  st.as_runner
