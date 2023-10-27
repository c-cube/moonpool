(* see: https://discuss.ocaml.org/t/a-hack-to-implement-efficient-tls-thread-local-storage/13264 *)

(* sanity check *)
let () = assert (Obj.field (Obj.repr (Thread.self ())) 1 = Obj.repr ())

type 'a key = {
  index: int;  (** Unique index for this key. *)
  compute: unit -> 'a;
      (** Initializer for values for this key. Called at most
        once per thread. *)
}

(** Counter used to allocate new keys *)
let counter = Atomic.make 0

(** Value used to detect a TLS slot that was not initialized yet *)
let sentinel_value_for_uninit_tls_ () : Obj.t = Obj.repr counter

let new_key compute : _ key =
  let index = Atomic.fetch_and_add counter 1 in
  { index; compute }

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
let[@inline never] grow_tls (old : Obj.t array) (index : int) : Obj.t array =
  let new_length = ceil_pow_2_minus_1 (index + 1) in
  let new_ = Array.make new_length (sentinel_value_for_uninit_tls_ ()) in
  Array.blit old 0 new_ 0 (Array.length old);
  new_

let[@inline] get_tls_ (index : int) : Obj.t array =
  let thread : thread_internal_state = Obj.magic (Thread.self ()) in
  let tls = thread.tls in
  if Obj.is_int tls then (
    let new_tls = grow_tls [||] index in
    thread.tls <- Obj.magic new_tls;
    new_tls
  ) else (
    let tls = (Obj.magic tls : Obj.t array) in
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
  if value != sentinel_value_for_uninit_tls_ () then
    Obj.magic value
  else (
    let value = key.compute () in
    Array.unsafe_set tls key.index (Obj.repr (Sys.opaque_identity value));
    value
  )

let set key value =
  let tls = get_tls_ key.index in
  Array.unsafe_set tls key.index (Obj.repr (Sys.opaque_identity value))
