(** Thread pool. *)

type t
(** A pool of threads. The pool contains a fixed number of threads that
    wait for work items to come, process these, and loop.

    If a pool is no longer needed, {!shutdown} can be used to signal all threads
    in it to stop (after they finish their work), and wait for them to stop.

    The threads are distributed across a fixed domain pool
    (whose size is determined by {!Domain.recommended_domain_count} on OCaml 5, and
    simple the single runtime on OCaml 4). *)

type thread_loop_wrapper =
  thread:Thread.t -> pool:t -> (unit -> unit) -> unit -> unit
(** A thread wrapper [f] takes the current thread, the current pool,
      and the worker function [loop : unit -> unit] which is
      the worker's main loop, and returns a new loop function.
      By default it just returns the same loop function but it can be used
      to install tracing, effect handlers, etc. *)

val add_global_thread_loop_wrapper : thread_loop_wrapper -> unit
(** [add_global_thread_loop_wrapper f] installs [f] to be installed in every new pool worker
      thread, for all existing pools, and all new pools created with [create].
      These wrappers accumulate: they all apply, but their order is not specified. *)

val create :
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?thread_wrappers:thread_loop_wrapper list ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'a) * (t -> 'a -> unit) ->
  ?min:int ->
  ?per_domain:int ->
  unit ->
  t
(** [create ()] makes a new thread pool.
     @param on_init_thread called at the beginning of each new thread
       in the pool.
     @param min minimum size of the pool. It will be at least [1] internally,
      so [0] or negative values make no sense.
     @param per_domain is the number of threads allocated per domain in the fixed
       domain pool. The default value is [0], but setting, say, [~per_domain:2]
       means that if there are [8] domains (which might be the case on an 8-core machine)
       then the minimum size of the pool is [16].
       If both [min] and [per_domain] are specified, the maximum of both
       [min] and [per_domain * num_of_domains] is used.
     @param on_exit_thread called at the end of each thread in the pool
     @param thread_wrappers a list of {!thread_loop_wrapper} functions
     to use for this pool's workers.
     @param around_task a pair of [before, after], where [before pool] is called
      before a task is processed,
      on the worker thread about to run it, and returns [x]; and [after pool x] is called by
      the same thread after the task is over. (since 0.2)
  *)

val size : t -> int
(** Number of threads *)

val num_tasks : t -> int
(** Current number of tasks. This is at best a snapshot, useful for metrics
    and debugging.
    @since 0.2 *)

val shutdown : t -> unit
(** Shutdown the pool and wait for it to terminate. Idempotent. *)

val shutdown_without_waiting : t -> unit
(** Shutdown the pool, and do not wait for it to terminate. Idempotent.
    @since 0.2 *)

exception Shutdown

val run : t -> (unit -> unit) -> unit
(** [run pool f] schedules [f] for later execution on the pool
    in one of the threads. [f()] will run on one of the pool's
    worker threads.
    @raise Shutdown if the pool was shut down before [run] was called. *)
