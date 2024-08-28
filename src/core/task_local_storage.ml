open Types_
(*
module A = Atomic_

type 'a key = 'a ls_key

let key_count_ = A.make 0

type t = local_storage
type ls_value += Dummy

let dummy : t = _dummy_ls

(** Resize array of TLS values *)
let[@inline never] resize_ (cur : ls_value array ref) n =
  if n > Sys.max_array_length then failwith "too many task local storage keys";
  let len = Array.length !cur in
  let new_ls =
    Array.make (min Sys.max_array_length (max n ((len * 2) + 2))) Dummy
  in
  Array.blit !cur 0 new_ls 0 len;
  cur := new_ls

module Direct = struct
  type nonrec t = t

  let create = create_local_storage
  let[@inline] copy (self : t) = ref (Array.copy !self)

  let get (type a) (self : t) ((module K) : a key) : a =
    if K.offset >= Array.length !self then resize_ self (K.offset + 1);
    match !self.(K.offset) with
    | K.V x -> (* common case first *) x
    | Dummy ->
      (* first time we access this *)
      let v = K.init () in
      !self.(K.offset) <- K.V v;
      v
    | _ -> assert false

  let set (type a) (self : t) ((module K) : a key) (v : a) : unit =
    assert (self != dummy);
    if K.offset >= Array.length !self then resize_ self (K.offset + 1);
    !self.(K.offset) <- K.V v;
    ()
end

let new_key (type t) ~init () : t key =
  let offset = A.fetch_and_add key_count_ 1 in
  (module struct
    type nonrec t = t
    type ls_value += V of t

    let offset = offset
    let init = init
  end : LS_KEY
    with type t = t)

let[@inline] get_cur_ () : ls_value array ref =
  match get_current_storage () with
  | Some r when r != dummy -> r
  | _ -> failwith "Task local storage must be accessed from within a runner."

let[@inline] get (key : 'a key) : 'a =
  let cur = get_cur_ () in
  Direct.get cur key

let[@inline] get_opt key =
  match get_current_storage () with
  | None -> None
  | Some cur -> Some (Direct.get cur key)

let[@inline] set key v : unit =
  let cur = get_cur_ () in
  Direct.set cur key v

let with_value key x f =
  let old = get key in
  set key x;
  Fun.protect ~finally:(fun () -> set key old) f

let get_current = get_current_storage
*)
