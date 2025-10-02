(** Lwt_engine-based event loop for Moonpool.

    In what follows, we mean by "lwt thread" the thread running {!lwt_main}
    (which wraps [Lwt_main.run]; so, the thread where the Lwt event loop and all
    Lwt callbacks execute).

    {b NOTE}: this is experimental and might change in future versions.

    @since 0.6

    The API has entirely changed since 0.9 , see
    https://github.com/c-cube/moonpool/pull/37 *)

module Fut = Moonpool.Fut

(** {2 Basic conversions} *)

val fut_of_lwt : 'a Lwt.t -> 'a Moonpool.Fut.t
(** [fut_of_lwt lwt_fut] makes a thread-safe moonpool future that completes when
    [lwt_fut] does. This can be run from any thread. *)

val lwt_of_fut : 'a Moonpool.Fut.t -> 'a Lwt.t
(** [lwt_of_fut fut] makes a lwt future that completes when [fut] does. This
    must be called from the Lwt thread, and the result must always be used only
    from inside the Lwt thread.
    @raise Failure if not run from the lwt thread. *)

(** {2 Helpers on the moonpool side} *)

val spawn_lwt : (unit -> 'a) -> 'a Lwt.t
(** This spawns a task that runs in the Lwt scheduler. This function is thread
    safe.
    @raise Failure if {!lwt_main} was not called. *)

val spawn_lwt_ignore : (unit -> unit) -> unit
(** Like {!spawn_lwt} but ignores the result, like [Lwt.async]. This function is
    thread safe. *)

val await_lwt : 'a Lwt.t -> 'a
(** [await_lwt fut] awaits a Lwt future from inside a task running on a moonpool
    runner. This must be run from within a Moonpool runner so that the await-ing
    effect is handled, but it doesn't have to run from inside the Lwt thread. *)

val run_in_lwt_and_await : (unit -> 'a) -> 'a
(** [run_in_lwt_and_await f] runs [f()] in the lwt thread, just like
    [spawn_lwt f], and then calls {!await_lwt} on the result. This means [f()]
    can use Lwt functions and libraries, use {!await_lwt} on them freely, etc.

    This function must run from within a task running on a moonpool runner so
    that it can [await_lwt]. *)

(** {2 Wrappers around Lwt_main} *)

val on_uncaught_exn : (Moonpool.Exn_bt.t -> unit) ref
(** Exception handler for tasks that raise an uncaught exception. *)

val lwt_main : (Moonpool.Runner.t -> 'a) -> 'a
(** [lwt_main f] sets the moonpool-lwt bridge up, runs lwt main, calls [f],
    destroys the bridge, and return the result of [f()]. Only one thread should
    call this at a time. *)

val on_lwt_thread : unit -> bool
(** [on_lwt_thread ()] is true if the current thread is the one currently
    running {!lwt_main}. This is thread safe.
    @raise Failure if {!lwt_main} was not called. *)

val lwt_main_runner : unit -> Moonpool.Runner.t
(** The runner from {!lwt_main}. The runner is only going to work if {!lwt_main}
    is currently running in some thread. This is thread safe.
    @raise Failure if {!lwt_main} was not called. *)

val is_setup : unit -> bool
(** Is the moonpool-lwt bridge setup? This is thread safe. *)
