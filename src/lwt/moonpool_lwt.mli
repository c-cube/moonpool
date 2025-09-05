(** Lwt_engine-based event loop for Moonpool.

    In what follows, we mean by "lwt thread" the thread running [Lwt_main.run]
    (so, the thread where the Lwt event loop and all Lwt callbacks execute).

    {b NOTE}: this is experimental and might change in future versions.

    @since 0.6 *)

module Fut = Moonpool.Fut

(** {2 Basic conversions} *)

val fut_of_lwt : 'a Lwt.t -> 'a Moonpool.Fut.t
(** [fut_of_lwt lwt_fut] makes a thread-safe moonpool future that completes when
    [lwt_fut] does. This must be run from within the Lwt thread. *)

val lwt_of_fut : 'a Moonpool.Fut.t -> 'a Lwt.t
(** [lwt_of_fut fut] makes a lwt future that completes when [fut] does. This
    must be called from the Lwt thread, and the result must always be used only
    from inside the Lwt thread.
    @raise Failure if not run from the lwt thread. *)

(** {2 Helpers on the moonpool side} *)

val spawn_lwt : (unit -> 'a) -> 'a Lwt.t
(** This spawns a task that runs in the Lwt scheduler.
    @raise Failure if {!lwt_main} was not called. *)

val await_lwt : 'a Lwt.t -> 'a
(** [await_lwt fut] awaits a Lwt future from inside a task running on a moonpool
    runner. This must be run from within a Moonpool runner so that the await-ing
    effect is handled. *)

val run_in_lwt_and_await : (unit -> 'a Lwt.t) -> 'a

(** {2 Wrappers around Lwt_main} *)

val on_uncaught_exn : (Moonpool.Exn_bt.t -> unit) ref

val lwt_main : (Moonpool.Runner.t -> 'a) -> 'a
(** [lwt_main f] sets the moonpool-lwt bridge up, runs lwt main, calls [f],
    destroys the bridge, and return the result of [f()]. *)

val lwt_main_runner : unit -> Moonpool.Runner.t
(** The runner from {!lwt_main}. The runner is only going to work if {!lwt_main}
    is currently running in some thread.
    @raise Failure if {!lwt_main} was not called. *)

val is_setup : unit -> bool
(** Is the moonpool-lwt bridge setup? *)
