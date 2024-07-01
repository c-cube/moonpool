(** Cancellation tokens *)

type t
(** A handle to cancel atomic actions (waiting on something), or
    stopping a subscription to some event. *)

val create : unit -> t
val create_with : (unit -> unit) -> t
val on_cancel : t -> (unit -> unit) -> unit
val on_cancel1 : t -> ('a -> unit) -> 'a -> unit

val cancel : t -> unit
(** Perform the cancellation. This should be idempotent. *)

val dummy : t
