(** Cooperative lock *)

type 'a t

val create : 'a -> 'a t
(** New lock *)

val with_lock : 'a t -> ('a -> 'b) -> 'b
(** [with_lock lock f]  calls [f] with the lock's content *)

val with_lock_update : 'a t -> ('a -> 'a * 'b) -> 'b
val with_try_lock : 'a t -> ('a option -> 'b) -> 'b
