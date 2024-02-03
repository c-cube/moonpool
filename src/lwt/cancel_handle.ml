(** Cancellation handle. *)

type t = { cancel: unit -> unit } [@@unboxed]
(** A handle to cancel atomic actions (waiting on something), or
    stopping a subscription to some event. *)

(** Perform the cancellation. This should be idempotent. *)
let[@inline] cancel self = self.cancel ()

(** Dummy that cancels nothing *)
let dummy : t = { cancel = ignore }

