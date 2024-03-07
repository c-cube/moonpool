open Common_
include Fuseau
module IO_unix = IO_unix
module Timer = Timer
module Net = Net

let main f =
  let loop = new U_loop.unix_ev_loop in
  let@ () = U_loop.with_cur loop in
  Fuseau.main ~loop:(loop :> Event_loop.t) f
