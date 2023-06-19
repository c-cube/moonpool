(** (Private) suspending tasks using Effects.

   This module is an implementation detail of Moonpool and should
   not be used outside of it. *)

open Suspend_types_

val suspend : suspension_handler -> unit
(** [suspend h] calls [h] with the current continuation [k].
    The suspension handler, [h], can decide to register [k] somewhere,
    so it's called later. *)

val with_suspend : run:runner -> (unit -> unit) -> unit
(** [with_suspend ~run f] runs [f()] in an environment where [suspend]
    will work. It passes [run] to suspension handlers. *)
