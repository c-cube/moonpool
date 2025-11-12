open Types_

open struct
  module FLS = Picos.Fiber.FLS
end

(** A local hmap, inherited in children fibers *)
let k_local_hmap : Hmap.t FLS.t = FLS.create ()

(** Access the local [hmap], or an empty one if not set *)
let[@inline] get_local_hmap () : Hmap.t =
  match TLS.get_exn k_cur_st with
  | exception TLS.Not_set -> Hmap.empty
  | { cur_fiber = fiber; _ } -> FLS.get fiber ~default:Hmap.empty k_local_hmap

let[@inline] set_local_hmap (h : Hmap.t) : unit =
  match TLS.get_exn k_cur_st with
  | exception TLS.Not_set -> ()
  | { cur_fiber = fiber; _ } -> FLS.set fiber k_local_hmap h

let[@inline] update_local_hmap (f : Hmap.t -> Hmap.t) : unit =
  match TLS.get_exn k_cur_st with
  | exception TLS.Not_set -> ()
  | { cur_fiber = fiber; _ } ->
    let h = FLS.get fiber ~default:Hmap.empty k_local_hmap in
    let h = f h in
    FLS.set fiber k_local_hmap h

(** @raise Invalid_argument if not present *)
let get_in_local_hmap_exn (k : 'a Hmap.key) : 'a =
  let h = get_local_hmap () in
  Hmap.get k h

let get_in_local_hmap_opt (k : 'a Hmap.key) : 'a option =
  let h = get_local_hmap () in
  Hmap.find k h

(** Remove given key from the local hmap *)
let[@inline] remove_in_local_hmap (k : _ Hmap.key) : unit =
  update_local_hmap (Hmap.rem k)

let[@inline] set_in_local_hmap (k : 'a Hmap.key) (v : 'a) : unit =
  update_local_hmap (Hmap.add k v)

(** [with_in_local_hmap k v f] calls [f()] in a context where [k] is bound to
    [v] in the local hmap. Then it restores the previous binding for [k]. *)
let with_in_local_hmap (k : 'a Hmap.key) (v : 'a) f =
  let h = get_local_hmap () in
  match Hmap.find k h with
  | None ->
    set_in_local_hmap k v;
    Fun.protect ~finally:(fun () -> remove_in_local_hmap k) f
  | Some old_v ->
    set_in_local_hmap k v;
    Fun.protect ~finally:(fun () -> set_in_local_hmap k old_v) f

(**/**)

(* private functions, to be used by the rest of moonpool *)
module Private_hmap_ls_ = struct
  (** Copy the hmap from f1.fls to f2.fls *)
  let copy_fls (f1 : Picos.Fiber.t) (f2 : Picos.Fiber.t) : unit =
    match FLS.get_exn f1 k_local_hmap with
    | exception FLS.Not_set -> ()
    | hmap -> FLS.set f2 k_local_hmap hmap
end

(**/**)
