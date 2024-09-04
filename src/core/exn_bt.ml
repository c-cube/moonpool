type t = exn * Printexc.raw_backtrace

let[@inline] make exn bt : t = exn, bt
let[@inline] exn (e, _) = e
let[@inline] bt (_, bt) = bt
let show self = Printexc.to_string (exn self)
let pp out self = Format.pp_print_string out (show self)
let[@inline] raise (e, bt) = Printexc.raise_with_backtrace e bt

let[@inline] get exn =
  let bt = Printexc.get_raw_backtrace () in
  make exn bt

let[@inline] get_callstack n exn =
  let bt = Printexc.get_callstack n in
  make exn bt

type nonrec 'a result = ('a, t) result

let[@inline] unwrap = function
  | Ok x -> x
  | Error ebt -> raise ebt
