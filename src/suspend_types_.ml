(** (Private) types for {!Suspend_}.

   This module is an implementation detail of Moonpool and should
   not be used outside of it. *)

type suspension = (unit, exn * Printexc.raw_backtrace) result -> unit
(** A suspended computation *)

type task = unit -> unit

type suspension_handler = {
  handle: run:(with_handler:bool -> task -> unit) -> suspension -> unit;
}
[@@unboxed]
(** The handler that knows what to do with the suspended computation *)
