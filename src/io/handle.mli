(** Handles.

    Each subscription has a unique handle that is used
    to cancel it and refer to it. *)

type handle_type =
  | H_read
  | H_write
  | H_timer

type t = private int
(** A handle. Its internal structure is unspecified. *)

val fresh : handle_type -> t
(** Get a fresh handle *)

val handle_type : t -> handle_type
(** Recover the type of the handle *)

module Map : Map.S with type key = t
module Tbl : Hashtbl.S with type key = t
