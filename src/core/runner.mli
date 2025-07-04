(** Interface for runners.

    This provides an abstraction for running tasks in the background, which is
    implemented by various thread pools.
    @since 0.3 *)

type fiber = Picos.Fiber.t
type task = unit -> unit

type t
(** A runner.

    If a runner is no longer needed, {!shutdown} can be used to signal all
    worker threads in it to stop (after they finish their work), and wait for
    them to stop.

    The threads are distributed across a fixed domain pool (whose size is
    determined by {!Domain.recommended_domain_count} on OCaml 5, and simple the
    single runtime on OCaml 4). *)

val size : t -> int
(** Number of threads/workers. *)

val num_tasks : t -> int
(** Current number of tasks. This is at best a snapshot, useful for metrics and
    debugging. *)

val shutdown : t -> unit
(** Shutdown the runner and wait for it to terminate. Idempotent. *)

val shutdown_without_waiting : t -> unit
(** Shutdown the pool, and do not wait for it to terminate. Idempotent. *)

exception Shutdown

val run_async : ?fiber:fiber -> t -> task -> unit
(** [run_async pool f] schedules [f] for later execution on the runner in one of
    the threads. [f()] will run on one of the runner's worker threads/domains.
    @param fiber if provided, run the task with this initial fiber data
    @raise Shutdown if the runner was shut down before [run_async] was called.
*)

val run_wait_block : ?fiber:fiber -> t -> (unit -> 'a) -> 'a
(** [run_wait_block pool f] schedules [f] for later execution on the pool, like
    {!run_async}. It then blocks the current thread until [f()] is done
    executing, and returns its result. If [f()] raises an exception, then
    [run_wait_block pool f] will raise it as well.

    {b NOTE} be careful with deadlocks (see notes in {!Fut.wait_block} about the
    required discipline to avoid deadlocks).
    @raise Shutdown if the runner was already shut down *)

val dummy : t
(** Runner that fails when scheduling tasks on it. Calling {!run_async} on it
    will raise Failure.
    @since 0.6 *)

(** {2 Implementing runners} *)

(** This module is specifically intended for users who implement their own
    runners. Regular users of Moonpool should not need to look at it. *)
module For_runner_implementors : sig
  val create :
    size:(unit -> int) ->
    num_tasks:(unit -> int) ->
    shutdown:(wait:bool -> unit -> unit) ->
    run_async:(fiber:fiber -> task -> unit) ->
    unit ->
    t
  (** Create a new runner.

      {b NOTE}: the runner should support DLA and {!Suspend_} on OCaml 5.x, so
      that {!Fork_join} and other 5.x features work properly. *)

  val k_cur_runner : t Thread_local_storage.t
  (** Key that should be used by each runner to store itself in TLS on every
      thread it controls, so that tasks running on these threads can access the
      runner. This is necessary for {!get_current_runner} to work. *)
end

val get_current_runner : unit -> t option
(** Access the current runner. This returns [Some r] if the call happens on a
    thread that belongs in a runner.
    @since 0.5 *)

val get_current_fiber : unit -> fiber option
(** [get_current_storage runner] gets the local storage for the currently
    running task. *)
