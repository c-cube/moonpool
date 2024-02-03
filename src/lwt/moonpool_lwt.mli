(** Lwt_engine-based event loop for Moonpool *)

module Cancel_handle = Cancel_handle
module Fiber = Moonpool_fib.Fiber
module FLS = Moonpool_fib.Fls

val fut_of_lwt : 'a Lwt.t -> 'a Moonpool.Fut.t

val await_lwt : 'a Lwt.t -> 'a
(** [await_lwt fut] awaits a Lwt future from inside a task running on
    a moonpool runner. *)

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

val main_with_runner : runner:Moonpool.Runner.t -> (unit -> 'a) -> 'a
(** [main_with_runner ~runner f] starts a Lwt-based event loop and runs [f()] inside
    a fiber in [runner]. *)

val main : (unit -> 'a) -> 'a
(** Like {!main_with_runner} but with a default choice of runner. *)
