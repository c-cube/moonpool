(** Thread pool. *)

type t
(** A pool of threads. *)

type thread_loop_wrapper =
  thread:Thread.t -> pool:t -> (unit -> unit) -> unit -> unit
(** a thread wrapper [f] takes the current thread, the current pool,
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
  ?min:int ->
  ?per_domain:int ->
  unit ->
  t
(** [create ()] makes a new thread pool.
     @param on_init_thread called at the beginning of each new thread
       in the pool.
       @param on_exit_thread called at the end of each thread in the pool
       @param thread_wrappers a list of {!thread_loop_wrapper} functions
       to use for this pool's workers.
  *)

val size : t -> int
(** Number of threads *)

val shutdown : t -> unit
(** Shutdown the pool and wait for it to terminate. Idempotent. *)

val run : t -> (unit -> unit) -> unit
(** [run pool f] schedules [f] for later execution on the pool
      in one of the threads. *)
