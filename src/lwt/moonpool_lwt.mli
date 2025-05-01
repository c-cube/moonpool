(** Lwt_engine-based event loop for Moonpool.

    In what follows, we mean by "lwt thread" the thread running [Lwt_main.run]
    (so, the thread where the Lwt event loop and all Lwt callbacks execute).

    {b NOTE}: this is experimental and might change in future versions.

    @since 0.6 *)

module Fiber = Moonpool_fib.Fiber
module FLS = Moonpool_fib.Fls

(** {2 Basic conversions} *)

val fut_of_lwt : 'a Lwt.t -> 'a Moonpool.Fut.t
(** [fut_of_lwt lwt_fut] makes a thread-safe moonpool future that completes when
    [lwt_fut] does. This must be run from within the Lwt thread. *)

val lwt_of_fut : 'a Moonpool.Fut.t -> 'a Lwt.t
(** [lwt_of_fut fut] makes a lwt future that completes when [fut] does. This
    must be called from the Lwt thread, and the result must always be used only
    from inside the Lwt thread. *)

(** {2 Helpers on the moonpool side} *)

val await_lwt : 'a Lwt.t -> 'a
(** [await_lwt fut] awaits a Lwt future from inside a task running on a moonpool
    runner. This must be run from within a Moonpool runner so that the await-ing
    effect is handled. *)

val run_in_lwt : (unit -> 'a Lwt.t) -> 'a Moonpool.Fut.t
(** [run_in_lwt f] runs [f()] from within the Lwt thread and returns a
    thread-safe future. This can be run from anywhere. *)

val run_in_lwt_and_await : (unit -> 'a Lwt.t) -> 'a
(** [run_in_lwt_and_await f] runs [f] in the Lwt thread, and awaits its result.
    Must be run from inside a moonpool runner so that the await-in effect is
    handled.

    This is similar to [Moonpool.await @@ run_in_lwt f]. *)

val get_runner : unit -> Moonpool.Runner.t
(** Returns the runner from within which this is called. Must be run from within
    a fiber.
    @raise Failure if not run within a fiber *)

(** {2 IO} *)

(** IO using the Lwt event loop.

    These IO operations work on non-blocking file descriptors and rely on a
    [Lwt_engine] event loop being active (meaning, [Lwt_main.run] is currently
    running in some thread).

    Calling these functions must be done from a moonpool runner. A function like
    [read] will first try to perform the IO action directly (here, call
    {!Unix.read}); if the action fails because the FD is not ready, then
    [await_readable] is called: it suspends the fiber and subscribes it to Lwt
    to be awakened when the FD becomes ready. *)
module IO : sig
  val read : Unix.file_descr -> bytes -> int -> int -> int
  (** Read from the file descriptor *)

  val await_readable : Unix.file_descr -> unit
  (** Suspend the fiber until the FD is readable *)

  val write_once : Unix.file_descr -> bytes -> int -> int -> int
  (** Perform one write into the file descriptor *)

  val await_writable : Unix.file_descr -> unit
  (** Suspend the fiber until the FD is writable *)

  val write : Unix.file_descr -> bytes -> int -> int -> unit
  (** Loop around {!write_once} to write the entire slice. *)

  val sleep_s : float -> unit
  (** Suspend the fiber for [n] seconds. *)
end

module IO_in = IO_in
(** Input channel *)

module IO_out = IO_out
(** Output channel *)

module TCP_server : sig
  type t = Lwt_io.server

  val establish_lwt :
    ?backlog:
      (* ?server_fd:Unix.file_descr -> *)
      int ->
    ?no_close:bool ->
    runner:Moonpool.Runner.t ->
    Unix.sockaddr ->
    (Unix.sockaddr -> Lwt_io.input_channel -> Lwt_io.output_channel -> unit) ->
    t
  (** [establish ~runner addr handler] runs a TCP server in the Lwt thread. When
      a client connects, a moonpool fiber is started on [runner] to handle it.
  *)

  val establish :
    ?backlog:
      (* ?server_fd:Unix.file_descr -> *)
      int ->
    ?no_close:bool ->
    runner:Moonpool.Runner.t ->
    Unix.sockaddr ->
    (Unix.sockaddr -> IO_in.t -> IO_out.t -> unit) ->
    t
  (** Like {!establish_lwt} but uses {!IO} to directly handle reads and writes
      on client sockets. *)

  val shutdown : t -> unit
  (** Shutdown the server *)
end

module TCP_client : sig
  val connect : Unix.sockaddr -> Unix.file_descr

  val with_connect : Unix.sockaddr -> (IO_in.t -> IO_out.t -> 'a) -> 'a
  (** Open a connection, and use {!IO} to read and write from the socket in a
      non blocking way. *)

  val with_connect_lwt :
    Unix.sockaddr -> (Lwt_io.input_channel -> Lwt_io.output_channel -> 'a) -> 'a
  (** Open a connection. *)
end

(** {2 Helpers on the lwt side} *)

val detach_in_runner : runner:Moonpool.Runner.t -> (unit -> 'a) -> 'a Lwt.t
(** [detach_in_runner ~runner f] runs [f] in the given moonpool runner, and
    returns a lwt future. This must be run from within the thread running
    [Lwt_main]. *)

(** {2 Wrappers around Lwt_main} *)

val main_with_runner : runner:Moonpool.Runner.t -> (unit -> 'a) -> 'a
(** [main_with_runner ~runner f] starts a Lwt-based event loop and runs [f()]
    inside a fiber in [runner]. *)

val main : (unit -> 'a) -> 'a
(** Like {!main_with_runner} but with a default choice of runner. *)
