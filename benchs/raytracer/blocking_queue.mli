(* vendored here *)

type 'a t

val create : unit -> _ t

exception Closed

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] into [q], and returns [()].
    @raise Closed if [close q] was previously called.*)

val pop : 'a t -> 'a
(** [pop q] pops the next element in [q]. It might block until an element comes.
   @raise Closed if the queue was closed before a new element was available.
   Note that calls to [pop] on a closed queue that still contains elements
   will succeed (until all elements are drained). *)

val close : _ t -> unit
(** Close the queue, meaning there won't be any more [push] allowed. *)
