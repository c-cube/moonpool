(** Fibers.

    A fiber is a lightweight computation that runs cooperatively
    alongside other fibers. In the context of moonpool, fibers
    have additional properties:

    - they run in a moonpool runner
    - they form a simple supervision tree, enabling a limited form
      of structured concurrency
*)

(**/**)

(**/**)

type 'a t
(** A fiber returning a value of type ['a]. *)

val res : 'a t -> 'a Fut.t
(** Future result of the fiber. *)

type 'a callback = 'a Exn_bt.result -> unit
(** Callbacks that are called when a fiber is done. *)

type cancel_callback = Exn_bt.t -> unit

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

val check_if_cancelled : unit -> unit
(** Check if the current fiber is cancelled, in which case this raises.
    Must be run from inside a fiber.
    @raise Failure if not. *)

val yield : unit -> unit
(** Yield control to the scheduler from the current fiber.
    @raise Failure if not run from inside a fiber. *)

exception Cancelled of Exn_bt.t
(** Exception for fibers that are cancelled. Polling points such
    as {!yield} and {!await} will raise this if the fiber has been cancelled. *)

val with_cancel_callback : _ t -> cancel_callback -> (unit -> 'a) -> 'a
(** [with_cancel_callback fib cb (fun () -> <e>)] evaluates [e]
    in a scope in which, if the fiber [fib] is cancelled,
    [cb()] is called. If [e] returns without the fiber being cancelled,
    this callback is removed. *)

val on_result : 'a t -> 'a callback -> unit
(** Wait for fiber to be done and call the callback
    with the result. If the fiber is done already then the
    callback is invoked immediately with its result. *)

val spawn_top : ?name:string -> on:Runner.t -> (unit -> 'a) -> 'a t
(** [spawn_top ~on f] spawns a new (toplevel) fiber onto the given runner.
    This fiber is not the child of any other fiber: its lifetime
    is only determined by the lifetime of [f()]. *)

val spawn_link : ?name:string -> protect:bool -> (unit -> 'a) -> 'a t
(** [spawn_link ~protect f] spawns a sub-fiber [f_child]
    from a running fiber [parent].
    The sub-fiber [f_child] is attached to the current fiber and fails
    if the current fiber [parent] fails.

    @param protect if true, when [f_child] fails, it does not
      affect [parent]. If false, [f_child] failing also
      causes [parent] to fail (and therefore all other children
      of [parent]).

    Must be run from inside a fiber.
    @raise Failure if not run from  inside a fiber. *)
