(** Exception with backtrace.

    @since 0.6 *)

type t = exn * Printexc.raw_backtrace
(** An exception bundled with a backtrace *)

val exn : t -> exn
val bt : t -> Printexc.raw_backtrace

val make : exn -> Printexc.raw_backtrace -> t
(** Trivial builder *)

val get : exn -> t
(** [get exn] is [make exn (get_raw_backtrace ())] *)

val get_callstack : int -> exn -> t

val raise : t -> 'a
(** Raise the exception with its save backtrace *)

val show : t -> string
(** Simple printing *)

val pp : Format.formatter -> t -> unit

type nonrec 'a result = ('a, t) result
