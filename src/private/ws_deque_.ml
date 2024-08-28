module A = Atomic_

(* terminology:

   - Bottom: where we push/pop normally. Only one thread can do that.
   - top: where work stealing happens (older values).
     This only ever grows.

   Elements are always added on the bottom end. *)

(** Circular array (size is [2 ^ log_size]) *)
module CA : sig
  type 'a t

  val create : dummy:'a -> unit -> 'a t
  val size : 'a t -> int
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
end = struct
  (** The array has size 256. *)
  let log_size = 8

  type 'a t = { arr: 'a array } [@@unboxed]

  let[@inline] size (_self : _ t) = 1 lsl log_size
  let create ~dummy () : _ t = { arr = Array.make (1 lsl log_size) dummy }

  let[@inline] get (self : 'a t) (i : int) : 'a =
    Array.unsafe_get self.arr (i land ((1 lsl log_size) - 1))

  let[@inline] set (self : 'a t) (i : int) (x : 'a) : unit =
    Array.unsafe_set self.arr (i land ((1 lsl log_size) - 1)) x
end

type 'a t = {
  top: int A.t;  (** Where we steal *)
  bottom: int A.t;  (** Where we push/pop from the owning thread *)
  mutable top_cached: int;  (** Last read value of [top] *)
  arr: 'a CA.t;  (** The circular array *)
}

let create ~dummy () : _ t =
  let top = A.make 0 in
  let arr = CA.create ~dummy () in
  (* allocate far from [top] to avoid false sharing *)
  let bottom = A.make 0 in
  { top; top_cached = 0; bottom; arr }

let[@inline] size (self : _ t) : int = max 0 (A.get self.bottom - A.get self.top)

exception Full

let push (self : 'a t) (x : 'a) : bool =
  try
    let b = A.get self.bottom in
    let t_approx = self.top_cached in

    (* Section 2.3: over-approximation of size.
       Only if it seems too big do we actually read [t]. *)
    let size_approx = b - t_approx in
    if size_approx >= CA.size self.arr - 1 then (
      (* we need to read the actual value of [top], which might entail contention. *)
      let t = A.get self.top in
      self.top_cached <- t;
      let size = b - t in

      if size >= CA.size self.arr - 1 then (* full! *) raise_notrace Full
    );

    CA.set self.arr b x;
    A.set self.bottom (b + 1);
    true
  with Full -> false

exception Empty

let pop_exn (self : 'a t) : 'a =
  let b = A.get self.bottom in
  let b = b - 1 in
  A.set self.bottom b;

  let t = A.get self.top in
  self.top_cached <- t;

  let size = b - t in
  if size < 0 then (
    (* reset to basic empty state *)
    A.set self.bottom t;
    raise_notrace Empty
  ) else if size > 0 then (
    (* can pop without modifying [top] *)
    let x = CA.get self.arr b in
    x
  ) else (
    assert (size = 0);
    (* there was exactly one slot, so we might be racing against stealers
       to update [self.top] *)
    if A.compare_and_set self.top t (t + 1) then (
      let x = CA.get self.arr b in
      A.set self.bottom (t + 1);
      x
    ) else (
      A.set self.bottom (t + 1);
      raise_notrace Empty
    )
  )

let[@inline] pop self : _ option =
  match pop_exn self with
  | exception Empty -> None
  | t -> Some t

let steal (self : 'a t) : 'a option =
  (* read [top], but do not update [top_cached]
     as we're in another thread *)
  let t = A.get self.top in
  let b = A.get self.bottom in

  let size = b - t in
  if size <= 0 then
    None
  else (
    let x = CA.get self.arr t in
    if A.compare_and_set self.top t (t + 1) then
      (* successfully increased top to consume [x] *)
      Some x
    else
      None
  )
