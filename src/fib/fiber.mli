(** Fibers.

    A fiber is a lightweight computation that runs cooperatively
    alongside other fibers. In the context of moonpool, fibers
    have additional properties:

    - they run in a moonpool runner
    - they form a simple supervision tree, enabling a limited form
      of structured concurrency
*)

type cancel_callback = Exn_bt.t -> unit
(** A callback used in case of cancellation *)

(** Nurseries.

  Fibers belong in a {b nursery}
  (as defined in {{: https://trio.readthedocs.io/en/stable/reference-core.html} Trio}).

  The nursery does multiple things.

  - it stores a runner, so we easily know where to run fibers;
  - it makes it clear in signatures that we might run fibers in a function
  - it groups cancellation of multiple fibers together

*)
module Nursery : sig
  type t

  val await : t -> unit
  (** Await for the nursery to exit. *)

  val with_create_top : on:Runner.t -> unit -> (t -> 'a) -> 'a
  (** New toplevel nursery. It runs fibers on the [on] runner. *)

  val with_create_sub : protect:bool -> t -> (t -> 'a) -> 'a
  (** Create a sub-nursery. The sub-nursery is cancelled if the
    parent is. Cancelling the sub-nursery also cancels the
    parent if [protect=false]. When the function returns,
    the nursery is closed an no other fiber can be scheduled on it. *)

  val cancel_with : t -> Exn_bt.t -> unit
  (** Cancel the nursery (and all its children) with the given exception. *)

  val with_cancel_callback : t -> cancel_callback -> (unit -> 'a) -> 'a
  (** [with_cancel_callback nursery cb (fun () -> <e>)] evaluates [e]
    in a scope in which, if the nursery [nursery] is cancelled,
    [cb()] is called. If [e] returns without the nursery being cancelled,
    this callback is removed. *)
end

(**/**)

module Private_ : sig
  type 'a state

  type 'a t = private {
    id: Handle.t;  (** unique identifier for this fiber *)
    state: 'a state Atomic.t;  (** Current state in the lifetime of the fiber *)
    res: 'a Fut.t;
    runner: Runner.t;
    ls: Task_local_storage.storage ref;
  }
  (** Type definition, exposed so that {!any} can be unboxed.
      Please do not rely on that. *)

  type any = Any : _ t -> any [@@unboxed]

  val get_cur : unit -> any option
end

(**/**)

type 'a t = 'a Private_.t
(** A fiber returning a value of type ['a]. *)

val res : 'a t -> 'a Fut.t
(** Future result of the fiber. *)

type 'a callback = 'a Exn_bt.result -> unit
(** Callbacks that are called when a fiber is done. *)

(** Type erased fiber *)
type any = Private_.any = Any : _ t -> any [@@unboxed]

val self : unit -> any
(** [self ()] is the current fiber.
    Must be run from inside a fiber.
    @raise Failure if not run from inside a fiber. *)

val cur_nursery : unit -> Nursery.t
(** [cur_nursery ()] returns the nearest nursery.
    Must be run from inside a fiber.
    @raise Failure if not run from inside a fiber. *)

val peek : 'a t -> 'a Fut.or_error option
(** Peek inside the future result *)

val is_done : _ t -> bool
(** Has the fiber completed? *)

val is_cancelled : _ t -> bool
(** Has the fiber completed with a failure? *)

val is_success : _ t -> bool
(** Has the fiber completed with a value? *)

val await : 'a t -> 'a
(** [await fib] is like [Fut.await (res fib)] *)

val wait_block_exn : 'a t -> 'a
(** [wait_block_exn fib] is [Fut.wait_block_exn (res fib)].
    {b NOTE}: See {!Fut.wait_block} for warnings about deadlocks. *)

val wait_block : 'a t -> 'a Fut.or_error
(** [wait_block fib] is [Fut.wait_block (res fib)].
    {b NOTE}: See {!Fut.wait_block} for warnings about deadlocks. *)

val check_if_cancelled : unit -> unit
(** Check if the current fiber is cancelled, in which case this raises.
    Must be run from inside a fiber.
    @raise Failure if not. *)

val yield : unit -> unit
(** Yield control to the scheduler from the current fiber.
    @raise Failure if not run from inside a fiber. *)

type cancel_handle
(** An opaque handle for a single cancel callback in a fiber *)

val add_on_cancel : _ t -> cancel_callback -> cancel_handle
(** [add_on_cancel fib cb] adds [cb] to the list of cancel callbacks
    for [fib]. If [fib] is already cancelled, [cb] is called immediately. *)

val remove_on_cancel : _ t -> cancel_handle -> unit
(** [remove_on_cancel fib h] removes the cancel callback
    associated with handle [h]. *)

val with_cancel_callback : _ t -> cancel_callback -> (unit -> 'a) -> 'a
(** [with_cancel_callback fib cb (fun () -> <e>)] evaluates [e]
    in a scope in which, if the fiber [fib] is cancelled,
    [cb()] is called. If [e] returns without the fiber being cancelled,
    this callback is removed. *)

val with_self_cancel_callback : cancel_callback -> (unit -> 'a) -> 'a
(** [with_self_cancel_callback cb f] calls [f()] in a scope where
    [cb] is added to the cancel callbacks of the current fiber *)

val on_result : 'a t -> 'a callback -> unit
(** Wait for fiber to be done and call the callback
    with the result. If the fiber is done already then the
    callback is invoked immediately with its result. *)

val spawn : Nursery.t -> ?protect:bool -> (Nursery.t -> 'a) -> 'a t
(** [spawn n f] spawns a new fiber [fib] in the given nursery [n].

    The fiber [fib] is attached to the nursery and fails
    if the nursery fails.

    The function [f] is passed a nursery whose lifetime is
    the same as the fiber's.

    @param protect if true, when [fib] fails, it does not
    affect [nursery] (but the failure can still be re-raised
    in {!await}). If false, [fib] failing also
    causes [nursery] to fail (and therefore all other children
    of [nursery] to be cancelled). Default is [true]. *)

val spawn_ignore : Nursery.t -> ?protect:bool -> (Nursery.t -> _) -> unit
(** [spawn_ignore n f] is [ignore (spawn n f)].
  The fiber will still affect termination of [n], ie. [n] will exit
  only after the fiber exits. *)
