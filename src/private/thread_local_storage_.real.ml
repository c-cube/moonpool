(* vendored from  https://github.com/c-cube/thread-local-storage *)

(* sanity check *)
let () = assert (Obj.field (Obj.repr (Thread.self ())) 1 = Obj.repr ())

type 'a t = int
(** Unique index for this TLS slot. *)

let tls_length index =
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
  in
  let size = ceil_pow_2_minus_1 (index + 1) in
  assert (size > index);
  size

(** Counter used to allocate new keys *)
let counter = Atomic.make 0

(** Value used to detect a TLS slot that was not initialized yet.
    Because [counter] is private and lives forever, no other
    object the user can see will have the same address. *)
let sentinel_value_for_uninit_tls : Obj.t = Obj.repr counter

external max_wosize : unit -> int = "caml_sys_const_max_wosize"

let max_word_size = max_wosize ()

let create () : _ t =
  let index = Atomic.fetch_and_add counter 1 in
  if tls_length index <= max_word_size then
    index
  else (
    (* Some platforms have a small max word size. *)
    ignore (Atomic.fetch_and_add counter (-1));
    failwith "Thread_local_storage.create: out of TLS slots"
  )

type thread_internal_state = {
  _id: int;  (** Thread ID (here for padding reasons) *)
  mutable tls: Obj.t;  (** Our data, stowed away in this unused field *)
  _other: Obj.t;
      (** Here to avoid lying to ocamlopt/flambda about the size of [Thread.t] *)
}
(** A partial representation of the internal type [Thread.t], allowing
  us to access the second field (unused after the thread
  has started) and stash TLS data in it. *)

let[@inline] get_raw index : Obj.t =
  let thread : thread_internal_state = Obj.magic (Thread.self ()) in
  let tls = thread.tls in
  if Obj.is_block tls && index < Array.length (Obj.obj tls : Obj.t array) then
    Array.unsafe_get (Obj.obj tls : Obj.t array) index
  else
    sentinel_value_for_uninit_tls

let[@inline never] tls_error () =
  failwith "Thread_local_storage.get: TLS entry not initialised"

let[@inline] get slot =
  let v = get_raw slot in
  if v != sentinel_value_for_uninit_tls then
    Obj.obj v
  else
    tls_error ()

let[@inline] get_opt slot =
  let v = get_raw slot in
  if v != sentinel_value_for_uninit_tls then
    Some (Obj.obj v)
  else
    None

(** Allocating and setting *)

(** Grow the array so that [index] is valid. *)
let grow (old : Obj.t array) (index : int) : Obj.t array =
  let new_length = tls_length index in
  let new_ = Array.make new_length sentinel_value_for_uninit_tls in
  Array.blit old 0 new_ 0 (Array.length old);
  new_

let get_tls_with_capacity index : Obj.t array =
  let thread : thread_internal_state = Obj.magic (Thread.self ()) in
  let tls = thread.tls in
  if Obj.is_int tls then (
    let new_tls = grow [||] index in
    thread.tls <- Obj.repr new_tls;
    new_tls
  ) else (
    let tls = (Obj.obj tls : Obj.t array) in
    if index < Array.length tls then
      tls
    else (
      let new_tls = grow tls index in
      thread.tls <- Obj.repr new_tls;
      new_tls
    )
  )

let[@inline] set slot value : unit =
  let tls = get_tls_with_capacity slot in
  Array.unsafe_set tls slot (Obj.repr (Sys.opaque_identity value))
