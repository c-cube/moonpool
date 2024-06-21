open Common_

type t = {
  fd: Unix.file_descr;
  closed: bool A.t;
}
(** A file descriptor with idempotent closing *)

let create (fd : Unix.file_descr) : t = { fd; closed = A.make false }
let[@inline] closed self = A.get self.closed

(** Close the file descriptor. Idempotent. *)
let[@inline] close (self : t) : unit =
  if not (A.exchange self.closed true) then Unix.close self.fd

let[@inline] close_noerr (self : t) : unit =
  if not (A.exchange self.closed true) then (
    try Unix.close self.fd with _ -> ()
  )
