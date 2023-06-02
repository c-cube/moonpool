(** Futures.

    A future of type ['a t] represents the result of a computation
    that will yield a value of type ['a].

    Typically, the computation is running on a thread pool {!Pool.t}
    and will proceed on some worker. Once set, a future cannot change.
    It either succeeds (storing a [Ok x] with [x: 'a]), or fail
    (storing a [Error (exn, bt)] with an exception and the corresponding
    backtrace).

    Combinators such as {!map} and {!join_array} can be used to produce
    futures from other futures (in a monadic way). Some combinators take
    a [pool] argument to specify where the intermediate computation takes
    place; for example [map ~pool ~f fut] maps the value in [fut]
    using function [f], applicatively; the call to [f] happens on
    pool [pool] (once [fut] resolves successfully with a value).
*)

type 'a or_error = ('a, exn * Printexc.raw_backtrace) result

type 'a t
(** A future with a result of type ['a]. *)

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
