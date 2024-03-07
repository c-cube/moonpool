(** Leftist Heaps

    Implementation following Okasaki's book. *)

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

module Make (E : PARTIAL_ORD) : S with type elt = E.t
