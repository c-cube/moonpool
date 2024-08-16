(** Thread local storage *)

type 'a t
(** A TLS slot for values of type ['a]. This allows the storage of a
    single value of type ['a] per thread. *)

val create : unit -> 'a t

val get : 'a t -> 'a
(** @raise Failure if not present *)

val get_opt : 'a t -> 'a option
val set : 'a t -> 'a -> unit
