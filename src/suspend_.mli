(** (Private) suspending tasks using Effects.

   This module is an implementation detail of Moonpool and should
   not be used outside of it. *)

open Suspend_types_

val suspend : suspension_handler -> unit
(** [suspend h] jumps back to the nearest {!with_suspend}
    and calls [h.handle] with the current continuation [k]
    and a task runner function.
*)

val with_suspend :
  run:(with_handler:bool -> task -> unit) -> (unit -> unit) -> unit
(** [with_suspend ~run f] runs [f()] in an environment where [suspend]
    will work. If [f()] suspends with suspension handler [h],
    this calls [h ~run k] where [k] is the suspension.
*)
