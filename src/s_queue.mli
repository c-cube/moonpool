(** Simple blocking queue *)

type 'a t

val create : unit -> _ t

exception Closed

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] into [q], and returns [()].
    @raise Closed if [close q] was previously called.*)

val pop : 'a t -> 'a
(** [pop q] pops the next element in [q]. It might block until an element comes.
   @raise Closed if the queue was closed before a new element was available. *)

val try_pop : 'a t -> 'a option
(** [try_pop q] immediately pops the first element of [q], if any,
    or returns [None] without blocking. *)

val close : _ t -> unit
(** Close the queue, meaning there won't be any more [push] allowed. *)
