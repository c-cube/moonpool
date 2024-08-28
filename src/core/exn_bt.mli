(** Exception with backtrace.

    Type changed @since NEXT_RELEASE

    @since 0.6 *)

(** An exception bundled with a backtrace *)
include module type of struct
  include Picos_exn_bt
end

val exn : t -> exn
val bt : t -> Printexc.raw_backtrace

val make : exn -> Printexc.raw_backtrace -> t
(** Trivial builder *)

val show : t -> string
(** Simple printing *)

val pp : Format.formatter -> t -> unit

type nonrec 'a result = ('a, t) result

val unwrap : 'a result -> 'a
(** [unwrap (Ok x)] is [x], [unwrap (Error ebt)] re-raises [ebt].
    @since NEXT_RELEASE *)
