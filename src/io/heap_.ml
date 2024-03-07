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

  val take_exn : t -> t * elt
  (** [take_exn h] is like {!take}, but can fail.
      @raise Empty if the heap is empty. *)
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

  let find_min = function
    | E -> None
    | N (_, x, _, _) -> Some x

  let take_exn = function
    | E -> raise Empty
    | N (_, x, l, r) -> merge l r, x
end
