(** Channels.

   Channels are pipelines of values where threads can push into
   one end, and pull from the other end.

   Unlike {!Moonpool.Blocking_queue}, channels are designed so
   that pushing never blocks, and pop'ing values returns a future.

   @since 0.3
*)

type 'a or_error = 'a Fut.or_error

type 'a t
(** Channel carrying values of type ['a]. *)

val create : unit -> 'a t
(** Create a channel. *)

exception Closed

val push : 'a t -> 'a -> unit
(** [push chan x] pushes [x] into [chan]. This does not block.
    @raise Closed if the channel is closed. *)

val pop : 'a t -> 'a Fut.t
(** Pop an element. This returns a future that will be
    fulfilled when an element is available.
    @raise Closed if the channel is closed, or fails the future
      if the channel is closed before an element is available for it. *)

val try_pop : 'a t -> 'a option
(** [try_pop chan] pops and return an element if one is available
    immediately. Otherwise it returns [None]. *)

val pop_block_exn : 'a t -> 'a
(** Like [pop], but blocks if an element is not available immediately.
    The precautions around blocking from inside a thread pool
    are the same as explained in {!Fut.wait_block}. *)

val close : _ t -> unit
(** Close the channel. Further push and pop calls will fail.
    This is idempotent. *)

[@@@ifge 5.0]

val pop_await : 'a t -> 'a
(** Like {!pop} but suspends the current thread until an element is
    available. See {!Fut.await} for more details.
    @since NEXT_RELEASE *)

[@@@endif]
