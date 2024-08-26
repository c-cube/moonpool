(** Exception with backtrace.

    Type changed @since NEXT_RELEASE

    @since 0.6 *)

include module type of Picos_exn_bt
(** An exception bundled with a backtrace *)

val exn : t -> exn
val bt : t -> Printexc.raw_backtrace

val make : exn -> Printexc.raw_backtrace -> t
(** Trivial builder *)

val show : t -> string
(** Simple printing *)

val pp : Format.formatter -> t -> unit

type nonrec 'a result = ('a, t) result
