(** Simple functional queue *)

type +'a t

val empty : 'a t
val return : 'a -> 'a t
val is_empty : _ t -> bool

exception Empty

val pop_exn : 'a t -> 'a * 'a t
val push : 'a t -> 'a -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
