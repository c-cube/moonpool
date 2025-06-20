(** Fibers.

    A fiber is a lightweight computation that runs cooperatively alongside other
    fibers. In the context of moonpool, fibers have additional properties:

    - they run in a moonpool runner
    - they form a simple supervision tree, enabling a limited form of structured
      concurrency *)

type cancel_callback = Exn_bt.t -> unit
(** A callback used in case of cancellation *)

(**/**)

(** Do not rely on this, it is internal implementation details. *)
module Private_ : sig
  type 'a state
  type pfiber

  type 'a t = private {
    id: Handle.t;  (** unique identifier for this fiber *)
    state: 'a state Atomic.t;  (** Current state in the lifetime of the fiber *)
    res: 'a Fut.t;
    runner: Runner.t;
    pfiber: pfiber;
  }
  (** Type definition, exposed so that {!any} can be unboxed. Please do not rely
      on that. *)

  type any = Any : _ t -> any [@@unboxed]

  exception Not_set

  val get_cur_exn : unit -> any
  (** [get_cur_exn ()] either returns the current fiber, or
      @raise Not_set if run outside a fiber. *)

  val get_cur_opt : unit -> any option
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

val return : 'a -> 'a t
val fail : Exn_bt.t -> _ t

val self : unit -> any
(** [self ()] is the current fiber. Must be run from inside a fiber.
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
(** [wait_block_exn fib] is [Fut.wait_block_exn (res fib)]. {b NOTE}: See
    {!Fut.wait_block} for warnings about deadlocks. *)

val wait_block : 'a t -> 'a Fut.or_error
(** [wait_block fib] is [Fut.wait_block (res fib)]. {b NOTE}: See
    {!Fut.wait_block} for warnings about deadlocks. *)

val check_if_cancelled : unit -> unit
(** Check if the current fiber is cancelled, in which case this raises. Must be
    run from inside a fiber.
    @raise e if the current fiber is cancelled with exception [e]
    @raise Failure if not run from a fiber. *)

val yield : unit -> unit
(** Yield control to the scheduler from the current fiber.
    @raise Failure if not run from inside a fiber. *)

type cancel_handle
(** An opaque handle for a single cancel callback in a fiber *)

val add_on_cancel : _ t -> cancel_callback -> cancel_handle
(** [add_on_cancel fib cb] adds [cb] to the list of cancel callbacks for [fib].
    If [fib] is already cancelled, [cb] is called immediately. *)

val remove_on_cancel : _ t -> cancel_handle -> unit
(** [remove_on_cancel fib h] removes the cancel callback associated with handle
    [h]. *)

val with_on_cancel : _ t -> cancel_callback -> (unit -> 'a) -> 'a
(** [with_on_cancel fib cb (fun () -> <e>)] evaluates [e] in a scope in which,
    if the fiber [fib] is cancelled, [cb()] is called. If [e] returns without
    the fiber being cancelled, this callback is removed. *)

val with_on_self_cancel : cancel_callback -> (unit -> 'a) -> 'a
(** [with_on_self_cancel cb f] calls [f()] in a scope where [cb] is added to the
    cancel callbacks of the current fiber; and [f()] terminates, [cb] is removed
    from the list. *)

val on_result : 'a t -> 'a callback -> unit
(** Wait for fiber to be done and call the callback with the result. If the
    fiber is done already then the callback is invoked immediately with its
    result. *)

val spawn_top : on:Runner.t -> (unit -> 'a) -> 'a t
(** [spawn_top ~on f] spawns a new (toplevel) fiber onto the given runner. This
    fiber is not the child of any other fiber: its lifetime is only determined
    by the lifetime of [f()]. *)

val spawn : ?on:Runner.t -> ?protect:bool -> (unit -> 'a) -> 'a t
(** [spawn ~protect f] spawns a sub-fiber [f_child] from a running fiber
    [parent]. The sub-fiber [f_child] is attached to the current fiber and fails
    if the current fiber [parent] fails.

    @param on
      if provided, start the fiber on the given runner. If not provided, use the
      parent's runner.
    @param protect
      if true, when [f_child] fails, it does not affect [parent]. If false,
      [f_child] failing also causes [parent] to fail (and therefore all other
      children of [parent]). Default is [true].

    Must be run from inside a fiber.
    @raise Failure if not run from inside a fiber. *)

val spawn_ignore : ?on:Runner.t -> ?protect:bool -> (unit -> _) -> unit
(** [spawn_ignore f] is [ignore (spawn f)]. The fiber will still affect
    termination of the parent, ie. the parent will exit only after this new
    fiber exits.
    @param on the optional runner to use, added since 0.7 *)

val spawn_top_ignore : on:Runner.t -> (unit -> _) -> unit
(** Like {!spawn_top} but ignores the result.
    @since 0.7 *)
