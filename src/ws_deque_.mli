(** Work-stealing deque.

  Adapted from "Dynamic circular work stealing deque", Chase & Lev
  *)

type 'a t
(** Deque containing values of type ['a] *)

val create : dummy:'a -> unit -> 'a t
(** Create a new deque. *)

val push : 'a t -> 'a -> bool
(** Push value at the bottom of deque. returns [true] if it succeeds.
    This must be called only by the owner thread. *)

val pop : 'a t -> 'a option
(** Pop value from the bottom of deque.
    This must be called only by the owner thread. *)

val steal : 'a t -> 'a option
(** Try to steal from the top of deque. This is thread-safe. *)

val size : _ t -> int
