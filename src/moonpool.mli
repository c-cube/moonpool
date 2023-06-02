(** Moonpool

  A pool within a bigger pool (ie the ocean). Here, we're talking about
  pools of [Thread.t] which live within a fixed pool of [Domain.t].
*)

type 'a or_error = ('a, exn * Printexc.raw_backtrace) result

(** Thread pool *)
module Pool : sig
  type t

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

  val shutdown : t -> unit
  (** Shutdown the pool and wait for it to terminate. Idempotent. *)

  val run : t -> (unit -> unit) -> unit
  (** [run pool f] schedules [f] for later execution on the pool
      in one of the threads. *)
end

val start_thread_on_some_domain : ('a -> unit) -> 'a -> Thread.t
(** Similar to {!Thread.create}, but it picks a background domain at random
    to run the thread. This ensures that we don't always pick the same domain
    to run all the various threads needed in an application (timers, event loops, etc.) *)

(** Futures *)
module Fut : sig
  type 'a t
  (** A future with a result of type ['a] *)

  type 'a promise
  (** A promise, which can be fulfilled exactly once to set
      the corresponding future *)

  val make : unit -> 'a t * 'a promise
  (** Make a new future with the associated promise *)

  val on_result : 'a t -> ('a or_error -> unit) -> unit
  (** [on_result fut f] registers [f] to be called in the future
      when [fut] is set ;
      or calls [f] immediately if [fut] is already set. *)

  exception Already_fulfilled

  val fulfill : 'a promise -> 'a or_error -> unit
  (** Fullfill the promise, setting the future at the same time.
      @raise Already_fulfilled if the promise is already fulfilled. *)

  val fulfill_idempotent : 'a promise -> 'a or_error -> unit
  (** Fullfill the promise, setting the future at the same time.
      Does nothing if the promise is already fulfilled. *)

  val return : 'a -> 'a t
  (** Already settled future, with a result *)

  val fail : exn -> Printexc.raw_backtrace -> _ t
  (** Already settled future, with a failure *)

  val of_result : 'a or_error -> 'a t

  val is_resolved : _ t -> bool
  (** [is_resolved fut] is [true] iff [fut] is resolved. *)

  val peek : 'a t -> 'a or_error option
  (** [peek fut] returns [Some r] if [fut] is currently resolved with [r],
      and [None] if [fut] is not resolved yet. *)

  (** {2 Combinators} *)

  val spawn : on:Pool.t -> (unit -> 'a) -> 'a t
  (** [spaw ~on f] runs [f()] on the given pool, and return a future that will
      hold its result. *)

  val map : ?on:Pool.t -> f:('a -> 'b) -> 'a t -> 'b t
  (** [map ?on ~f fut] returns a new future [fut2] that resolves
      with [f x] if [fut] resolved with [x];
      and fails with [e] if [fut] fails with [e] or [f x] raises [e].
      @param on if provided, [f] runs on the given pool *)

  val bind : ?on:Pool.t -> f:('a -> 'b t) -> 'a t -> 'b t
  (** [map ?on ~f fut] returns a new future [fut2] that resolves
      like the future [f x] if [fut] resolved with [x];
    and fails with [e] if [fut] fails with [e] or [f x] raises [e].
      @param on if provided, [f] runs on the given pool *)

  val both : 'a t -> 'b t -> ('a * 'b) t
  (** [both a b] succeeds with [x, y] if [a] succeeds with [x] and
      [b] succeeds with [y], or fails if any of them fails. *)

  val choose : 'a t -> 'b t -> ('a, 'b) Either.t t
  (** [choose a b] succeeds [Left x] or [Right y] if [a] succeeds with [x] or
      [b] succeeds with [y], or fails if both of them fails.
      If they both succeed, it is not specified which result is used. *)

  val choose_same : 'a t -> 'a t -> 'a t
  (** [choose_same a b] succeeds with the value of one of [a] or [b] if
      they succeed, or fails if both fail.
      If they both succeed, it is not specified which result is used. *)

  val join_array : 'a t array -> 'a array t
  (** Wait for all the futures in the array. Fails if any future fails. *)

  val join_list : 'a t list -> 'a list t
  (** Wait for all the futures in the list. Fails if any future fails. *)

  val wait_array : _ t array -> unit t
  (** [wait_array arr] waits for all futures in [arr] to resolve. It discards
      the individual results of futures in [arr]. It fails if any future fails. *)

  val wait_list : _ t list -> unit t
  (** [wait_list l] waits for all futures in [l] to resolve. It discards
      the individual results of futures in [l]. It fails if any future fails. *)

  val for_ : on:Pool.t -> int -> (int -> unit) -> unit t
  (** [for_ ~on n f] runs [f 0], [f 1], …, [f (n-1)] on the pool, and returns
      a future that resolves when all the tasks have resolved, or fails
      as soon as one task has failed. *)

  (** {2 Blocking} *)

  val wait_block : 'a t -> 'a or_error
  (** [wait_block fut] blocks the current thread until [fut] is resolved,
      and returns its value.

      A word of warning: this will monopolize the calling thread until the future
      resolves. This can also easily cause deadlocks, if enough threads in a pool
      call [wait_block] on futures running on the same pool or a pool depending on it.

      A good rule to avoid deadlocks is to run this from outside of any pool,
      or to have an acyclic order between pools where [wait_block]
      is only called from a pool on futures evaluated in a pool that comes lower
      in the hierarchy.
      If this rule is broken, it is possible for all threads in a pool to wait
      for futures that can only make progress on these same threads,
      hence the deadlock.
  *)

  val wait_block_exn : 'a t -> 'a
  (** Same as {!wait_block} but re-raises the exception if the future failed. *)

  module type INFIX = sig
    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  end

  include INFIX
  (** Operators that run on the same thread *)

  (** Make infix combinators *)
  module Infix (_ : sig
    val pool : Pool.t
  end) : INFIX
end
