open Types_
module PF = Picos.Fiber

type 'a t = 'a PF.FLS.t

exception Not_set = PF.FLS.Not_set

let create = PF.FLS.create

let[@inline] get_exn k =
  let fiber = get_current_fiber_exn () in
  PF.FLS.get_exn fiber k

let get_opt k =
  match get_current_fiber () with
  | None -> None
  | Some fiber ->
    (match PF.FLS.get_exn fiber k with
    | x -> Some x
    | exception Not_set -> None)

let[@inline] get k ~default =
  match get_current_fiber () with
  | None -> None
  | Some fiber -> PF.FLS.get fiber ~default k

let[@inline] set k v : unit =
  let fiber = get_current_fiber_exn () in
  PF.FLS.set fiber k v

let with_value k v (f : _ -> 'b) : 'b =
  let fiber = get_current_fiber_exn () in

  match PF.FLS.get_exn fiber k with
  | exception Not_set ->
    PF.FLS.set fiber k v;
    (* nothing to restore back to, just call [f] *)
    f ()
  | old_v ->
    PF.FLS.set fiber k v;
    let finally () = PF.FLS.set fiber k old_v in
    Fun.protect f ~finally

include Hmap_ls_
