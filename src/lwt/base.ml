open Common_
module Fiber = Moonpool_fib.Fiber
module FLS = Moonpool_fib.Fls

(** Action scheduled from outside the loop *)
module Action = struct
  type event = Lwt_engine.event
  type cb = event -> unit

  (** Action that we ask the lwt loop to perform, from the outside *)
  type t =
    | Wait_readable of Unix.file_descr * cb
    | Wait_writable of Unix.file_descr * cb
    | Sleep of float * bool * cb
    (* TODO: provide actions with cancellation, alongside a "select" operation *)
    (* | Cancel of event *)
    | On_termination : 'a Lwt.t * ('a Exn_bt.result -> unit) -> t
    | Wakeup : 'a Lwt.u * 'a -> t
    | Wakeup_exn : _ Lwt.u * exn -> t
    | Other of (unit -> unit)

  (** Perform the action from within the Lwt thread *)
  let perform (self : t) : unit =
    match self with
    | Wait_readable (fd, cb) -> ignore (Lwt_engine.on_readable fd cb : event)
    | Wait_writable (fd, cb) -> ignore (Lwt_engine.on_writable fd cb : event)
    | Sleep (f, repeat, cb) -> ignore (Lwt_engine.on_timer f repeat cb : event)
    (* | Cancel ev -> Lwt_engine.stop_event ev *)
    | On_termination (fut, f) ->
      Lwt.on_any fut
        (fun x -> f @@ Ok x)
        (fun exn -> f @@ Error (Exn_bt.get_callstack 10 exn))
    | Wakeup (prom, x) -> Lwt.wakeup prom x
    | Wakeup_exn (prom, e) -> Lwt.wakeup_exn prom e
    | Other f -> f ()
end

module Action_queue = struct
  type t = { q: Action.t list Atomic.t } [@@unboxed]

  let create () : t = { q = Atomic.make [] }
  let pop_all (self : t) : _ list = Atomic.exchange self.q []

  (** Push the action and return whether the queue was previously empty *)
  let push (self : t) (a : Action.t) : bool =
    let is_first = ref true in
    while
      let old = Atomic.get self.q in
      if Atomic.compare_and_set self.q old (a :: old) then (
        is_first := old == [];
        false
      ) else
        true
    do
      ()
    done;
    !is_first
end

module Perform_action_in_lwt = struct
  open struct
    let actions_ : Action_queue.t = Action_queue.create ()

    (** Gets the current set of notifications and perform them from inside the
    Lwt thread *)
    let perform_pending_actions () : unit =
      let@ _sp =
        Moonpool.Private.Tracing_.with_span
          "moonpool-lwt.perform-pending-actions"
      in

      let l = Action_queue.pop_all actions_ in
      List.iter Action.perform l

    let notification : int =
      Lwt_unix.make_notification ~once:false perform_pending_actions
  end

  let schedule (a : Action.t) : unit =
    let is_first = Action_queue.push actions_ a in
    if is_first then Lwt_unix.send_notification notification
end

let get_runner () : M.Runner.t =
  match M.Runner.get_current_runner () with
  | Some r -> r
  | None -> failwith "Moonpool_lwt.get_runner: not inside a runner"

let lwt_of_fut (fut : 'a M.Fut.t) : 'a Lwt.t =
  let lwt_fut, lwt_prom = Lwt.wait () in
  M.Fut.on_result fut (function
    | Ok x -> Perform_action_in_lwt.schedule @@ Action.Wakeup (lwt_prom, x)
    | Error (exn, _) ->
      Perform_action_in_lwt.schedule @@ Action.Wakeup_exn (lwt_prom, exn));
  lwt_fut

let fut_of_lwt (lwt_fut : _ Lwt.t) : _ M.Fut.t =
  match Lwt.poll lwt_fut with
  | Some x -> M.Fut.return x
  | None ->
    let fut, prom = M.Fut.make () in
    Lwt.on_any lwt_fut
      (fun x -> M.Fut.fulfill prom (Ok x))
      (fun e -> M.Fut.fulfill prom (Error (e, Printexc.get_callstack 10)));
    fut

let await_lwt (fut : _ Lwt.t) =
  match Lwt.poll fut with
  | Some x -> x
  | None ->
    (* suspend fiber, wake it up when [fut] resolves *)
    M.Private.Suspend_.suspend
      {
        handle =
          (fun ~run:_ ~resume sus ->
            let on_lwt_done _ = resume sus @@ Ok () in
            Perform_action_in_lwt.(
              schedule Action.(On_termination (fut, on_lwt_done))));
      };

    (match Lwt.poll fut with
    | Some x -> x
    | None -> assert false)

let run_in_lwt f : _ M.Fut.t =
  let fut, prom = M.Fut.make () in
  Perform_action_in_lwt.schedule
    (Action.Other
       (fun () ->
         let lwt_fut = f () in
         Lwt.on_any lwt_fut
           (fun x -> M.Fut.fulfill prom @@ Ok x)
           (fun exn -> M.Fut.fulfill prom @@ Error (Exn_bt.get exn))));
  fut

let run_in_lwt_and_await f = M.Fut.await @@ run_in_lwt f

let detach_in_runner ~runner f : _ Lwt.t =
  let fut, promise = Lwt.wait () in
  M.Runner.run_async runner (fun () ->
      match f () with
      | x -> Perform_action_in_lwt.schedule @@ Action.Wakeup (promise, x)
      | exception exn ->
        Perform_action_in_lwt.schedule @@ Action.Wakeup_exn (promise, exn));
  fut

let main_with_runner ~runner (f : Fiber.Nursery.t -> 'a) : 'a =
  let lwt_fut, lwt_prom = Lwt.wait () in
  let@ n = Fiber.Nursery.with_create_top ~on:runner () in

  let _fiber =
    Fiber.spawn n (fun _n ->
        try
          let x = f n in
          Perform_action_in_lwt.schedule (Action.Wakeup (lwt_prom, x))
        with exn ->
          Perform_action_in_lwt.schedule (Action.Wakeup_exn (lwt_prom, exn)))
  in

  Lwt_main.run lwt_fut

let main f =
  let@ runner = M.Ws_pool.with_ () in
  main_with_runner ~runner f
