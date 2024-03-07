module type PARTIAL_ORD = sig
  type t

  val leq : t -> t -> bool
  (** [leq x y] shall return [true] iff [x] is lower or equal to [y]. *)
end

module type S = sig
  type elt
  type t

  val empty : t
  (** [empty] returns the empty heap. *)

  val is_empty : t -> bool
  (** [is_empty h] returns [true] if the heap [h] is empty. *)

  exception Empty

  val merge : t -> t -> t
  (** [merge h1 h2] merges the two heaps [h1] and [h2]. *)

  val insert : elt -> t -> t
  (** [insert x h] inserts an element [x] into the heap [h]. *)

  val find_min : t -> elt option
  (** [find_min h] find the minimal element of the heap [h]. *)

  val find_min_exn : t -> elt
  (** [find_min_exn h] is like {!find_min} but can fail.
      @raise Empty if the heap is empty. *)

  val take : t -> (t * elt) option
  (** [take h] extracts and returns the minimum element, and the new heap (without
      this element), or [None] if the heap [h] is empty. *)

  val take_exn : t -> t * elt
  (** [take_exn h] is like {!take}, but can fail.
      @raise Empty if the heap is empty. *)

  val delete_one : (elt -> elt -> bool) -> elt -> t -> t
  (** [delete_one eq x h] uses [eq] to find one occurrence of a value [x]
      if it exist in the heap [h], and delete it.
      If [h] do not contain [x] then it return [h]. *)

  val size : t -> int
end

module Make (E : PARTIAL_ORD) : S with type elt = E.t = struct
  type elt = E.t

  type t =
    | E
    | N of int * elt * t * t

  let empty = E

  let is_empty = function
    | E -> true
    | N _ -> false

  exception Empty

  (* Rank of the tree *)
  let _rank = function
    | E -> 0
    | N (r, _, _, _) -> r

  (* Make a balanced node labelled with [x], and subtrees [a] and [b].
     We ensure that the right child's rank is ≤ to the rank of the
     left child (leftist property). The rank of the resulting node
     is the length of the rightmost path. *)
  let _make_node x a b =
    if _rank a >= _rank b then
      N (_rank b + 1, x, a, b)
    else
      N (_rank a + 1, x, b, a)

  let rec merge t1 t2 =
    match t1, t2 with
    | t, E -> t
    | E, t -> t
    | N (_, x, a1, b1), N (_, y, a2, b2) ->
      if E.leq x y then
        _make_node x a1 (merge b1 t2)
      else
        _make_node y a2 (merge t1 b2)

  let insert x h = merge (N (1, x, E, E)) h

  let find_min_exn = function
    | E -> raise Empty
    | N (_, x, _, _) -> x

  let find_min = function
    | E -> None
    | N (_, x, _, _) -> Some x

  let take = function
    | E -> None
    | N (_, x, l, r) -> Some (merge l r, x)

  let take_exn = function
    | E -> raise Empty
    | N (_, x, l, r) -> merge l r, x

  let delete_one eq x h =
    let rec aux = function
      | E -> false, E
      | N (_, y, l, r) as h ->
        if eq x y then
          true, merge l r
        else if E.leq y x then (
          let found_left, l1 = aux l in
          let found, r1 =
            if found_left then
              true, r
            else
              aux r
          in
          if found then
            true, _make_node y l1 r1
          else
            false, h
        ) else
          false, h
    in
    snd (aux h)

  let rec size = function
    | E -> 0
    | N (_, _, l, r) -> 1 + size l + size r
end
