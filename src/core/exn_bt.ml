type t = exn * Printexc.raw_backtrace

let[@inline] make exn bt : t = exn, bt
let[@inline] exn (e, _) = e
let[@inline] bt (_, bt) = bt

let[@inline] get exn =
  let bt = Printexc.get_raw_backtrace () in
  make exn bt

let[@inline] get_callstack n exn =
  let bt = Printexc.get_callstack n in
  make exn bt

let show self = Printexc.to_string (fst self)
let[@inline] raise self = Printexc.raise_with_backtrace (exn self) (bt self)

type nonrec 'a result = ('a, t) result
