(** (Private) suspending tasks using Effects.

   This module is an implementation detail of Moonpool and should
   not be used outside of it, except by experts to implement {!Runner}. *)

type suspension = unit Exn_bt.result -> unit
(** A suspended computation *)

type task = unit -> unit

type suspension_handler = {
  handle:
    run:(task -> unit) ->
    resume:(suspension -> unit Exn_bt.result -> unit) ->
    suspension ->
    unit;
}
[@@unboxed]
(** The handler that knows what to do with the suspended computation.

   The handler is given a few things:

   - the suspended computation (which can be resumed with a result
     eventually);
   - a [run] function that can be used to start tasks to perform some
    computation.
   - a [resume] function to resume the suspended computation. This
    must be called exactly once, in all situations.

  This means that a fork-join primitive, for example, can use a single call
  to {!suspend} to:
    - suspend the caller until the fork-join is done
    - use [run] to start all the tasks. Typically [run] is called multiple times,
      which is where the "fork" part comes from. Each call to [run] potentially
      runs in parallel with the other calls. The calls must coordinate so
      that, once they are all done, the suspended caller is resumed with the
      aggregated result of the computation.
    - use [resume] exactly
*)

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

type _ Effect.t +=
  | Suspend : suspension_handler -> unit Effect.t
        (** The effect used to suspend the current thread and pass it, suspended,
    to the handler. The handler will ensure that the suspension is resumed later
    once some computation has been done. *)
  | Yield : unit Effect.t
        (** The effect used to interrupt the current computation and immediately re-schedule
      it on the same runner. *)

[@@@ocaml.alert "+unstable"]

val yield : unit -> unit
(** Interrupt current computation, and re-schedule it at the end of the
    runner's job queue. *)

val suspend : suspension_handler -> unit
(** [suspend h] jumps back to the nearest {!with_suspend}
    and calls [h.handle] with the current continuation [k]
    and a task runner function.
*)

[@@@endif]

type with_suspend_handler =
  | WSH : {
      on_suspend: unit -> 'state;
          (** on_suspend called when [f()] suspends itself. *)
      run: 'state -> task -> unit;  (** run used to schedule new tasks *)
      resume: 'state -> suspension -> unit Exn_bt.result -> unit;
          (** resume run the suspension. Must be called exactly once. *)
    }
      -> with_suspend_handler

val with_suspend : with_suspend_handler -> (unit -> unit) -> unit
(** [with_suspend wsh f]
    runs [f()] in an environment where [suspend] will work.

    If [f()] suspends with suspension handler [h],
    this calls [wsh.on_suspend()] to capture the current state [st].
    Then [h.handle ~st ~run ~resume k] is called, where [k] is the suspension.
    The suspension should always be passed exactly once to
    [resume]. [run] should be used to start other tasks.
*)
