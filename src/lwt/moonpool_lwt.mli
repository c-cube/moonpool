(** Lwt_engine-based event loop for Moonpool *)

module Fiber = Moonpool_fib.Fiber
module FLS = Moonpool_fib.Fls

(** {2 Basic conversions} *)

val fut_of_lwt : 'a Lwt.t -> 'a Moonpool.Fut.t
(** [fut_of_lwt lwt_fut] makes a thread-safe moonpool future that
    completes when [lwt_fut] does *)

val lwt_of_fut : 'a Moonpool.Fut.t -> 'a Lwt.t
(** [lwt_of_fut fut] makes a lwt future that completes when
    [fut] does. The result should be used only from inside the
    thread running [Lwt_main.run]. *)

(** {2 Helpers on the moonpool side} *)

val await_lwt : 'a Lwt.t -> 'a
(** [await_lwt fut] awaits a Lwt future from inside a task running on
    a moonpool runner. This must be run from within moonpool. *)

val run_in_lwt : (unit -> 'a Lwt.t) -> 'a Moonpool.Fut.t
(** [run_in_lwt f] runs [f()] from within the Lwt thread
    and returns a thread-safe future. *)

val run_in_lwt_and_await : (unit -> 'a Lwt.t) -> 'a
(** [run_in_lwt_and_await f] runs [f] in the Lwt thread, and
    awaits its result. Must be run from inside a moonpool runner. *)

val get_runner : unit -> Moonpool.Runner.t
(** Returns the runner from within which this is called.
    Must be run from within a fiber.
    @raise Failure if not run within a fiber *)

(** {2 IO} *)

(** IO using the Lwt event loop.

    These IO operations work on non-blocking file descriptors
    and rely on a [Lwt_engine] event loop being active (meaning,
    [Lwt_main.run] is currently running in some thread).

    Calling these functions must be done from a moonpool runner and
    will suspend the current task/fut/fiber if the FD is not ready.
*)
module IO : sig
  val read : Unix.file_descr -> bytes -> int -> int -> int
  val write_once : Unix.file_descr -> bytes -> int -> int -> int
  val write : Unix.file_descr -> bytes -> int -> int -> unit
  val sleep_s : float -> unit
end

module IO_in = IO_in
module IO_out = IO_out

module TCP_server : sig
  type t = Lwt_io.server

  val establish :
    ?backlog:(* ?server_fd:Unix.file_descr -> *)
             int ->
    ?no_close:bool ->
    runner:Moonpool.Runner.t ->
    Unix.sockaddr ->
    (Unix.sockaddr -> IO_in.t -> IO_out.t -> unit) ->
    t

  val shutdown : t -> unit
end

module TCP_client : sig
  val with_connect : Unix.sockaddr -> (IO_in.t -> IO_out.t -> 'a) -> 'a
  (** Open a connection. *)
end

(** {2 Helpers on the lwt side} *)

val detach_in_runner : runner:Moonpool.Runner.t -> (unit -> 'a) -> 'a Lwt.t
(** [detach_in_runner ~runner f] runs [f] in the given moonpool runner,
    and returns a lwt future. This must be run from within the thread
    running [Lwt_main]. *)

(** {2 Wrappers around Lwt_main} *)

val main_with_runner : runner:Moonpool.Runner.t -> (unit -> 'a) -> 'a
(** [main_with_runner ~runner f] starts a Lwt-based event loop and runs [f()] inside
    a fiber in [runner]. *)

val main : (unit -> 'a) -> 'a
(** Like {!main_with_runner} but with a default choice of runner. *)
