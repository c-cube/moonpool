(** Futures.

    A future of type ['a t] represents the result of a computation
    that will yield a value of type ['a].

    Typically, the computation is running on a thread pool {!Runner.t}
    and will proceed on some worker. Once set, a future cannot change.
    It either succeeds (storing a [Ok x] with [x: 'a]), or fail
    (storing a [Error (exn, bt)] with an exception and the corresponding
    backtrace).

    Combinators such as {!map} and {!join_array} can be used to produce
    futures from other futures (in a monadic way). Some combinators take
    a [on] argument to specify a runner on which the intermediate computation takes
    place; for example [map ~on:pool ~f fut] maps the value in [fut]
    using function [f], applicatively; the call to [f] happens on
    the runner [pool] (once [fut] resolves successfully with a value).
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

exception Not_ready
(** @since 0.2 *)

val get_or_fail : 'a t -> 'a or_error
(** [get_or_fail fut] obtains the result from [fut] if it's fulfilled
    (i.e. if [peek fut] returns [Some res], [get_or_fail fut] returns [res]).
    @raise Not_ready if the future is not ready.
    @since 0.2 *)

val get_or_fail_exn : 'a t -> 'a
(** [get_or_fail_exn fut] obtains the result from [fut] if it's fulfilled,
    like {!get_or_fail}. If the result is an [Error _], the exception inside
    is re-raised.
    @raise Not_ready if the future is not ready.
    @since 0.2 *)

val is_done : _ t -> bool
(** Is the future resolved? This is the same as [peek fut |> Option.is_some].
    @since 0.2 *)

(** {2 Combinators} *)

val spawn : on:Runner.t -> (unit -> 'a) -> 'a t
(** [spaw ~on f] runs [f()] on the given runner [on], and return a future that will
      hold its result. *)

val spawn_on_current_runner : (unit -> 'a) -> 'a t
(** This must be run from inside a runner, and schedules
    the new task on it as well.

    See {!Runner.get_current_runner} to see how the runner is found.

    @since 0.5
    @raise Failure if run from outside a runner. *)

val reify_error : 'a t -> 'a or_error t
(** [reify_error fut] turns a failing future into a non-failing
    one  that contain [Error (exn, bt)]. A non-failing future
    returning [x] is turned into [Ok x]
    @since 0.4 *)

val map : ?on:Runner.t -> f:('a -> 'b) -> 'a t -> 'b t
(** [map ?on ~f fut] returns a new future [fut2] that resolves
      with [f x] if [fut] resolved with [x];
      and fails with [e] if [fut] fails with [e] or [f x] raises [e].
      @param on if provided, [f] runs on the given runner *)

val bind : ?on:Runner.t -> f:('a -> 'b t) -> 'a t -> 'b t
(** [bind ?on ~f fut] returns a new future [fut2] that resolves
      like the future [f x] if [fut] resolved with [x];
    and fails with [e] if [fut] fails with [e] or [f x] raises [e].
      @param on if provided, [f] runs on the given runner *)

val bind_reify_error : ?on:Runner.t -> f:('a or_error -> 'b t) -> 'a t -> 'b t
(** [bind_reify_error ?on ~f fut] returns a new future [fut2] that resolves
      like the future [f (Ok x)] if [fut] resolved with [x];
      and resolves like the future [f (Error (exn, bt))]
      if [fut] fails with [exn] and backtrace [bt].
    @param on if provided, [f] runs on the given runner
    @since 0.4 *)

val join : 'a t t -> 'a t
(** [join fut] is [fut >>= Fun.id]. It joins the inner layer of the future.
    @since 0.2 *)

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

val for_ : on:Runner.t -> int -> (int -> unit) -> unit t
(** [for_ ~on n f] runs [f 0], [f 1], …, [f (n-1)] on the runner, and returns
      a future that resolves when all the tasks have resolved, or fails
      as soon as one task has failed. *)

val for_array : on:Runner.t -> 'a array -> (int -> 'a -> unit) -> unit t
(** [for_array ~on arr f] runs [f 0 arr.(0)], …, [f (n-1) arr.(n-1)] in
    the runner (where [n = Array.length arr]), and returns a future
    that resolves when all the tasks are done,
    or fails if any of them fails.
    @since 0.2 *)

val for_list : on:Runner.t -> 'a list -> ('a -> unit) -> unit t
(** [for_list ~on l f] is like [for_array ~on (Array.of_list l) f].
    @since 0.2 *)

(** {2 Await}

    {b NOTE} This is only available on OCaml 5. *)

[@@@ifge 5.0]

val await : 'a t -> 'a
(** [await fut] suspends the current tasks until [fut] is fulfilled, then
    resumes the task on this same runner.

    @since 0.3

    This must only be run from inside the runner itself. The runner must
    support {!Suspend_}.
    {b NOTE}: only on OCaml 5.x
*)

[@@@endif]

(** {2 Blocking} *)

val wait_block : 'a t -> 'a or_error
(** [wait_block fut] blocks the current thread until [fut] is resolved,
      and returns its value.

      {b NOTE}: A word of warning: this will monopolize the calling thread until the future
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

(** {2 Infix operators}

    These combinators run on either the current pool (if present),
    or on the same thread that just fulfilled the previous future
    if not.

    They were previously present as [module Infix_local] and [val infix],
    but are now simplified.

    @since 0.5 *)

(** @since 0.5 *)
module Infix : sig
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

include module type of Infix

module Infix_local = Infix
[@@deprecated "Use Infix"]
(** @deprecated use Infix instead *)
