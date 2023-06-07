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

val try_pop : force_lock:bool -> 'a t -> 'a option
(** [try_pop q] immediately pops the first element of [q], if any,
    or returns [None] without blocking.
    @param force_lock if true, use {!Mutex.lock} (which can block under contention);
      if false, use {!Mutex.try_lock}, which might return [None] even in
    presence of an element if there's contention *)

val try_push : 'a t -> 'a -> bool
(** [try_push q x] tries to push into [q], in which case
    it returns [true]; or it fails to push and returns [false]
    without blocking.
    @raise Closed if the locking succeeded but the queue is closed.
*)

val close : _ t -> unit
(** Close the queue, meaning there won't be any more [push] allowed. *)
