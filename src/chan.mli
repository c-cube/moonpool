(** Channels.

   Channels are pipelines of values where threads can push into
   one end, and pull from the other end. *)

type 'a or_error = 'a Fut.or_error

type 'a t
(** Channel carrying values of type ['a] *)

val create : unit -> 'a t
(** Create a channel. *)

exception Closed

val push : 'a t -> 'a -> unit
(** [push chan x] pushes [x] into [chan].
    @raise Closed if the channel is closed. *)

val pop : 'a t -> 'a Fut.t
(** Pop an element.
    @raise Closed if the channel is closed. *)

val try_pop : 'a t -> 'a option

val pop_block_exn : 'a t -> 'a
(** Like [pop], but block if an element is not available immediately. *)

val close : _ t -> unit
(** Close the channel. Further push and pop calls will fail.
    This is idempotent. *)
