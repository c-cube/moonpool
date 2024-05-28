type 'a t = {
  hd: 'a list;
  tl: 'a list;
}
(** Queue containing elements of type 'a.

      invariant: if hd=[], then tl=[] *)

let[@inline] list_is_empty_ = function
  | [] -> true
  | _ :: _ -> false

let empty = { hd = []; tl = [] }
let[@inline] return x : _ t = { hd = [ x ]; tl = [] }

let[@inline] make_ hd tl =
  match hd with
  | [] -> { hd = List.rev tl; tl = [] }
  | _ :: _ -> { hd; tl }

let[@inline] is_empty self = list_is_empty_ self.hd
let[@inline] push self x : _ t = make_ self.hd (x :: self.tl)

let iter f (self : _ t) : unit =
  List.iter f self.hd;
  List.iter f self.tl

exception Empty

let pop_exn self =
  match self.hd with
  | [] ->
    assert (list_is_empty_ self.tl);
    raise Empty
  | x :: hd' ->
    let self' = make_ hd' self.tl in
    x, self'
