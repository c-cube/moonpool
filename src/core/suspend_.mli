(** (Private) suspending tasks using Effects.

   This module is an implementation detail of Moonpool and should
   not be used outside of it, except by experts to implement {!Runner}. *)

type suspension = (unit, exn * Printexc.raw_backtrace) result -> unit
(** A suspended computation *)

type task = unit -> unit

type suspension_handler = {
  handle: name:string -> run:(name:string -> task -> unit) -> suspension -> unit;
}
[@@unboxed]
(** The handler that knows what to do with the suspended computation.

   The handler is given a few things:

   - the name (if any) of the current computation
   - the suspended computation (which can be resumed with a result
     eventually);
   - a [run] function that can be used to start tasks to perform some
    computation.

  This means that a fork-join primitive, for example, can use a single call
  to {!suspend} to:
    - suspend the caller until the fork-join is done
    - use [run] to start all the tasks. Typically [run] is called multiple times,
      which is where the "fork" part comes from. Each call to [run] potentially
      runs in parallel with the other calls. The calls must coordinate so
      that, once they are all done, the suspended caller is resumed with the
      aggregated result of the computation.
*)

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

type _ Effect.t +=
  | Suspend : suspension_handler -> unit Effect.t
        (** The effect used to suspend the current thread and pass it, suspended,
    to the handler. The handler will ensure that the suspension is resumed later
    once some computation has been done. *)

[@@@ocaml.alert "+unstable"]

val suspend : suspension_handler -> unit
(** [suspend h] jumps back to the nearest {!with_suspend}
    and calls [h.handle] with the current continuation [k]
    and a task runner function.
*)

[@@@endif]

val with_suspend :
  name:string ->
  on_suspend:(unit -> unit) ->
  run:(name:string -> task -> unit) ->
  (unit -> unit) ->
  unit
(** [with_suspend ~run f] runs [f()] in an environment where [suspend]
    will work. If [f()] suspends with suspension handler [h],
    this calls [h ~run k] where [k] is the suspension.
    The suspension should always run in a new task, via [run].

    @param on_suspend called when [f()] suspends itself.

    This will not do anything on OCaml 4.x.
*)
