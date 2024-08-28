include Picos_exn_bt

let[@inline] make exn bt : t = { exn; bt }
let[@inline] exn self = self.exn
let[@inline] bt self = self.bt
let show self = Printexc.to_string (exn self)
let pp out self = Format.pp_print_string out (show self)

type nonrec 'a result = ('a, t) result

let[@inline] unwrap = function
  | Ok x -> x
  | Error ebt -> raise ebt
