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
  val shrink : 'a t -> bottom:int -> top:int -> 'a t
end = struct
  type 'a t = {
    log_size: int;
    arr: 'a option array;
  }

  let[@inline] size (self : _ t) = 1 lsl self.log_size

  let create ~log_size () : _ t =
    { log_size; arr = Array.make (1 lsl log_size) None }

  let[@inline] get (self : _ t) (i : int) : 'a =
    match Array.unsafe_get self.arr (i land ((1 lsl self.log_size) - 1)) with
    | Some x -> x
    | None -> assert false

  let[@inline] set (self : 'a t) (i : int) (x : 'a) : unit =
    Array.unsafe_set self.arr (i land ((1 lsl self.log_size) - 1)) (Some x)

  let grow (self : _ t) ~bottom ~top : 'a t =
    let new_arr = create ~log_size:(self.log_size + 1) () in
    for i = top to bottom - 1 do
      set new_arr i (get self i)
    done;
    new_arr

  let shrink (self : _ t) ~bottom ~top : 'a t =
    let new_arr = create ~log_size:(self.log_size - 1) () in
    for i = top to bottom - 1 do
      set new_arr i (get self i)
    done;
    new_arr
end

type 'a t = {
  top: int A.t;  (** Where we steal *)
  bottom: int A.t;  (** Where we push/pop from the owning thread *)
  mutable top_cached: int;  (** Last read value of [top] *)
  arr: 'a CA.t A.t;  (** The circular array *)
}

let create () : _ t =
  let top = A.make 0 in
  let arr = A.make @@ CA.create ~log_size:4 () in
  (* allocate far from top to avoid false sharing *)
  let bottom = A.make 0 in
  { top; top_cached = 0; bottom; arr }

let[@inline] size (self : _ t) : int = max 0 (A.get self.bottom - A.get self.top)

let push (self : 'a t) (x : 'a) : unit =
  let b = A.get self.bottom in
  let t_approx = self.top_cached in
  let arr = ref (A.get self.arr) in

  (* Section 2.3: over-approximation of size.
     Only if it seems too big do we actually read [t]. *)
  let size_approx = b - t_approx in
  if size_approx >= CA.size !arr - 1 then (
    (* we need to read the actual value of [top], which might entail contention. *)
    let t = A.get self.top in
    self.top_cached <- t;
    let size = b - t in

    if size >= CA.size !arr - 1 then (
      arr := CA.grow !arr ~top:t ~bottom:b;
      A.set self.arr !arr
    )
  );

  CA.set !arr b x;
  A.set self.bottom (b + 1)

let maybe_shrink_ (self : _ t) arr ~top ~bottom : unit =
  let size = bottom - top in
  let ca_size = CA.size arr in
  if ca_size >= 256 && size < ca_size / 3 then
    A.set self.arr (CA.shrink arr ~top ~bottom)

let pop (self : 'a t) : 'a option =
  let b = A.get self.bottom in
  let arr = A.get self.arr in
  let b = b - 1 in
  A.set self.bottom b;

  let t = A.get self.top in
  self.top_cached <- t;

  let size = b - t in
  if size < 0 then (
    (* reset to basic empty state *)
    A.set self.bottom t;
    None
  ) else if size > 0 then (
    (* can pop without modifying [top] *)
    let x = CA.get arr b in
    maybe_shrink_ self arr ~bottom:b ~top:t;
    Some x
  ) else (
    assert (size = 0);
    (* there was exactly one slot, so we might be racing against stealers
       to update [self.top] *)
    if A.compare_and_set self.top t (t + 1) then (
      let x = CA.get arr b in
      A.set self.bottom (t + 1);
      Some x
    ) else (
      A.set self.bottom (t + 1);
      None
    )
  )

let steal (self : 'a t) : 'a option =
  (* read [top], but do not update [top_cached]
     as we're in another thread *)
  let t = A.get self.top in
  let b = A.get self.bottom in
  let arr = A.get self.arr in

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
