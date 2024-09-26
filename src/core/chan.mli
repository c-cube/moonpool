(** Channels.

    The channels have bounded size. Push/pop return futures or can use effects
    to provide an [await]-friendly version.

    The channels became bounded since @NEXT_RELEASE .
*)

type 'a t
(** Channel carrying values of type ['a]. *)

val create : max_size:int -> unit -> 'a t
(** Create a channel. *)

exception Closed

val try_push : 'a t -> 'a -> bool
(** [try_push chan x] pushes [x] into [chan]. This does not block.
    Returns [true] if it succeeded in pushing.
    @raise Closed if the channel is closed. *)

val try_pop : 'a t -> 'a option
(** [try_pop chan] pops and return an element if one is available
    immediately. Otherwise it returns [None].
    @raise Closed if the channel is closed and empty.
    *)

val close : _ t -> unit
(** Close the channel. Further push and pop calls will fail.
    This is idempotent. *)

[@@@ifge 5.0]

val push : 'a t -> 'a -> unit
(** Push the value into the channel, suspending the current task
    if the channel is currently full.
    @raise Closed if the channel is closed
    @since NEXT_RELEASE *)

val pop : 'a t -> 'a
(** Pop an element. This might suspend the current task if the
    channel is currently empty.
    @raise Closed if the channel is empty and closed.
    @since NEXT_RELEASE *)

(*
val pop_block_exn : 'a t -> 'a
(** Like [pop], but blocks if an element is not available immediately.
    The precautions around blocking from inside a thread pool
    are the same as explained in {!Fut.wait_block}. *)
*)

[@@@endif]
