(** Work-stealing deque.

  Adapted from "Dynamic circular work stealing deque", Chase & Lev
  *)

type 'a t
(** Deque containing values of type ['a] *)

val create : unit -> _ t
(** Create a new deque. *)

val push : 'a t -> 'a -> unit
(** Push value at the bottom of deque. This is not thread-safe. *)

val pop : 'a t -> 'a option
(** Pop value from the bottom of deque. This is not thread-safe. *)

val steal : 'a t -> 'a option
(** Try to steal from the top of deque. This is thread-safe. *)

val size : _ t -> int
