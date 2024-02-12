open Types_
module A = Atomic_

type 'a key = 'a ls_key

let key_count_ = A.make 0

type storage = task_ls

let new_key (type t) ~init () : t key =
  let offset = A.fetch_and_add key_count_ 1 in
  (module struct
    type nonrec t = t
    type ls_value += V of t

    let offset = offset
    let init = init
  end : LS_KEY
    with type t = t)

type ls_value += Dummy

(** Resize array of TLS values *)
let[@inline never] resize_ (cur : ls_value array ref) n =
  if n > Sys.max_array_length then failwith "too many task local storage keys";
  let len = Array.length !cur in
  let new_ls =
    Array.make (min Sys.max_array_length (max n ((len * 2) + 2))) Dummy
  in
  Array.blit !cur 0 new_ls 0 len;
  cur := new_ls

let[@inline] get_cur_ () : ls_value array ref =
  match TLS.get k_ls_values with
  | Some r -> r
  | None -> failwith "Task local storage must be accessed from within a runner."

let get (type a) ((module K) : a key) : a =
  let cur = get_cur_ () in
  if K.offset >= Array.length !cur then resize_ cur (K.offset + 1);
  match !cur.(K.offset) with
  | K.V x -> (* common case first *) x
  | Dummy ->
    (* first time we access this *)
    let v = K.init () in
    !cur.(K.offset) <- K.V v;
    v
  | _ -> assert false

let set (type a) ((module K) : a key) (v : a) : unit =
  let cur = get_cur_ () in
  if K.offset >= Array.length !cur then resize_ cur (K.offset + 1);
  !cur.(K.offset) <- K.V v;
  ()

let with_value key x f =
  let old = get key in
  set key x;
  Fun.protect ~finally:(fun () -> set key old) f

module Private_ = struct
  module Storage = struct
    type t = storage

    let k_storage = k_ls_values
    let[@inline] create () = [||]
    let copy = Array.copy
    let dummy = [||]
  end
end
