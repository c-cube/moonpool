module A = Atomic_

(* terminology:

   - Bottom: where we push/pop normally. Only one thread can do that.
   - top: where work stealing happens (older values).
     This only ever grows.

   Elements are always added on the bottom end. *)

(** Circular array (size is [2 ^ log_size]) *)
module CA : sig
  type 'a t

  val create : log_size:int -> unit -> 'a t
  val size : _ t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val grow : 'a t -> bottom:int -> top:int -> 'a t
end = struct
  type 'a t = {
    log_size: int;
    arr: 'a option array;
  }

  let[@inline] size (self : _ t) = 1 lsl self.log_size

  let create ~log_size () : _ t =
    { log_size; arr = Array.make (1 lsl log_size) None }

  let[@inline] get (self : _ t) (i : int) : 'a =
    match Array.unsafe_get self.arr (i mod size self) with
    | Some x -> x
    | None -> assert false

  let[@inline] set (self : 'a t) (i : int) (x : 'a) : unit =
    Array.unsafe_set self.arr (i mod size self) (Some x)

  let grow (self : _ t) ~bottom ~top : 'a t =
    let new_arr = create ~log_size:(self.log_size + 1) () in
    for i = top to bottom - 1 do
      set new_arr i (get self i)
    done;
    new_arr
end

type 'a t = {
  top: int A.t;  (** Where we steal *)
  bottom: int A.t;  (** Where we push/pop from the owning thread *)
  mutable arr: 'a CA.t;  (** The circular array *)
}

let create () : _ t =
  let arr = CA.create ~log_size:4 () in
  { top = A.make 0; bottom = A.make 0; arr }

let[@inline] size (self : _ t) : int = max 0 (A.get self.bottom - A.get self.top)

let push (self : 'a t) (x : 'a) : unit =
  let b = A.get self.bottom in
  let t = A.get self.top in
  let size = b - t in

  if size >= CA.size self.arr - 1 then
    self.arr <- CA.grow self.arr ~top:t ~bottom:b;

  CA.set self.arr b x;
  A.set self.bottom (b + 1)

let pop (self : 'a t) : 'a option =
  let b = A.get self.bottom in
  let arr = self.arr in
  let b = b - 1 in
  A.set self.bottom b;
  let t = A.get self.top in
  let size = b - t in
  if size < 0 then (
    A.set self.bottom t;
    None
  ) else if size > 0 then (
    let x = CA.get arr b in
    Some x
  ) else if A.compare_and_set self.top t (t + 1) then (
    (* exactly one slot, so we might be racing against stealers
       to update [self.top] *)
    let x = CA.get arr b in
    A.set self.bottom (t + 1);
    Some x
  ) else
    None

let steal (self : 'a t) : 'a option =
  let t = A.get self.top in
  let b = A.get self.bottom in
  let arr = self.arr in

  let size = b - t in
  if size <= 0 then
    None
  else (
    let x = CA.get arr t in
    if A.compare_and_set self.top t (t + 1) then
      (* successfully increased top to consume [x] *)
      Some x
    else
      None
  )
