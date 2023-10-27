(* see: https://discuss.ocaml.org/t/a-hack-to-implement-efficient-tls-thread-local-storage/13264 *)

module A = Atomic_

(* sanity check *)
let () = assert (Obj.field (Obj.repr (Thread.self ())) 1 = Obj.repr ())

type view = ..
type view += Sentinel

type 'a key = {
  index: int;  (** Unique index for this key. *)
  unwrap: view -> 'a option;
  wrap: 'a -> view;
  compute: unit -> 'a;
      (** Initializer for values for this key. Called at most
        once per thread. *)
}

(** Counter used to allocate new keys *)
let counter = A.make 0

let new_key (type a) compute : a key =
  let module M = struct
    type view += V of a
  end in
  let index = A.fetch_and_add counter 1 in
  let wrap x = M.V x in
  let unwrap = function
    | M.V x -> Some x
    | _ -> None
  in
  { index; compute; wrap; unwrap }

type thread_internal_state = {
  _id: int;  (** Thread ID (here for padding reasons) *)
  mutable tls: Obj.t;  (** Our data, stowed away in this unused field *)
}
(** A partial representation of the internal type [Thread.t], allowing
  us to access the second field (unused after the thread
  has started) and stash TLS data in it. *)

let ceil_pow_2_minus_1 (n : int) : int =
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  if Sys.int_size > 32 then
    n lor (n lsr 32)
  else
    n

(** Grow the array so that [index] is valid. *)
let[@inline never] grow_tls (old : view array) (index : int) : view array =
  let new_length = ceil_pow_2_minus_1 (index + 1) in
  let new_ = Array.make new_length Sentinel in
  Array.blit old 0 new_ 0 (Array.length old);
  new_

let[@inline] get_tls_ (index : int) : view array =
  let thread : thread_internal_state = Obj.magic (Thread.self ()) in
  let tls = thread.tls in
  if Obj.is_int tls then (
    let new_tls = grow_tls [||] index in
    thread.tls <- Obj.magic new_tls;
    new_tls
  ) else (
    let tls = (Obj.magic tls : view array) in
    if index < Array.length tls then
      tls
    else (
      let new_tls = grow_tls tls index in
      thread.tls <- Obj.magic new_tls;
      new_tls
    )
  )

let get key =
  let tls = get_tls_ key.index in
  let value = Array.unsafe_get tls key.index in

  match key.unwrap value with
  | Some v -> v
  | None ->
    (match value with
    | Sentinel ->
      let value = key.compute () in
      Array.unsafe_set tls key.index (key.wrap value);
      value
    | _ -> assert false)

let set key value =
  let tls = get_tls_ key.index in
  Array.unsafe_set tls key.index (key.wrap value)
