(** The unique name of a fiber.

    Each fiber has a unique handle that can be used to refer to it in maps or
    sets. *)

type t = private int
(** Unique, opaque identifier for a fiber. *)

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val generate_fresh : unit -> t
(** Generate a fresh, unique identifier *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
