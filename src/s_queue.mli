(** Simple blocking queue *)

type 'a t

val create : unit -> _ t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
