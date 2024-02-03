module M = Moonpool
module Exn_bt = M.Exn_bt

let ( let@ ) = ( @@ )

let[@inline] cancel_handle_of_event (ev : Lwt_engine.event) : Cancel_handle.t =
  let cancel () = Lwt_engine.stop_event ev in
  { Cancel_handle.cancel }
