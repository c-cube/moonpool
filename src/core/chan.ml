module A = Atomic_

let[@inline] list_is_empty_ = function
  | [] -> true
  | _ :: _ -> false

(** Simple functional queue *)
module Q : sig
  type 'a t

  val empty : 'a t
  val is_empty : _ t -> bool

  exception Empty

  val pop_exn : 'a t -> 'a * 'a t
  val push : 'a t -> 'a -> 'a t
  val iter : ('a -> unit) -> 'a t -> unit
end = struct
  type 'a t = {
    hd: 'a list;
    tl: 'a list;
  }
  (** Queue containing elements of type 'a.

      invariant: if hd=[], then tl=[] *)

  let empty = { hd = []; tl = [] }

  let[@inline] make_ hd tl =
    match hd with
    | [] -> { hd = List.rev tl; tl = [] }
    | _ :: _ -> { hd; tl }

  let[@inline] is_empty self = list_is_empty_ self.hd
  let[@inline] push self x : _ t = make_ self.hd (x :: self.tl)

  let iter f (self : _ t) : unit =
    List.iter f self.hd;
    List.iter f self.tl

  exception Empty

  let pop_exn self =
    match self.hd with
    | [] ->
      assert (list_is_empty_ self.tl);
      raise Empty
    | x :: hd' ->
      let self' = make_ hd' self.tl in
      x, self'
end

exception Closed
exception Full

module State = struct
  type 'a t = {
    q: 'a Q.t;
    size: int;
    pop_waiters: Trigger.t Q.t;
    push_waiters: Trigger.t Q.t;
  }

  (** @raise Q.Empty *)
  let[@inline] pop_one_ ~max_size (st : 'a t) : 'a * 'a t * Trigger.t Q.t =
    let x, new_q = Q.pop_exn st.q in
    let new_st = { st with q = new_q; size = st.size - 1 } in
    if st.size = max_size then
      (* we signal all the push waiters, the channel isn't full anymore *)
      x, { new_st with push_waiters = Q.empty }, st.push_waiters
    else
      x, new_st, Q.empty

  (** @raise Full *)
  let[@inline] push_one_ ~max_size (st : 'a t) (x : 'a) : 'a t * Trigger.t Q.t =
    if st.size >= max_size then raise_notrace Full;
    let new_q = Q.push st.q x in
    let new_st = { st with q = new_q; size = st.size + 1 } in
    if st.size = 0 then
      (* we signal all the pop waiters, the channel isn't empty anymore *)
      { new_st with pop_waiters = Q.empty }, st.pop_waiters
    else
      new_st, Q.empty
end

type 'a t = {
  st: 'a State.t A.t;
  closed: bool A.t;
  max_size: int;
}

let create ~max_size () : _ t =
  if max_size < 0 then invalid_arg "Chan: max_size < 0";
  {
    max_size;
    closed = A.make false;
    st =
      A.make
        {
          State.q = Q.empty;
          size = 0;
          pop_waiters = Q.empty;
          push_waiters = Q.empty;
        };
  }

let try_pop (self : 'a t) : 'a option =
  let old_st = A.get self.st in
  match State.pop_one_ ~max_size:self.max_size old_st with
  | exception Q.Empty ->
    if A.get self.closed then raise Closed;
    None
  | x, new_st, to_broadcast ->
    if A.compare_and_set self.st old_st new_st then (
      Q.iter Trigger.signal to_broadcast;
      Some x
    ) else
      None

let try_push (self : 'a t) (x : 'a) : bool =
  if A.get self.closed then raise Closed;
  let old_st = A.get self.st in
  match State.push_one_ ~max_size:self.max_size old_st x with
  | exception Full -> false
  | new_st, to_broadcast ->
    if A.compare_and_set self.st old_st new_st then (
      Q.iter Trigger.signal to_broadcast;
      true
    ) else
      false

let close (self : _ t) : unit =
  if not (A.exchange self.closed true) then
    while
      let old_st = A.get self.st in
      if
        A.compare_and_set self.st old_st
          { old_st with push_waiters = Q.empty; pop_waiters = Q.empty }
      then (
        (* signal all waiters *)
        Q.iter Trigger.signal old_st.push_waiters;
        Q.iter Trigger.signal old_st.pop_waiters;

        false
      ) else
        true
    do
      Domain_.relax ()
    done

[@@@ifge 5.0]

let rec pop (self : 'a t) : 'a =
  let old_st = A.get self.st in
  match State.pop_one_ ~max_size:self.max_size old_st with
  | exception Q.Empty ->
    if A.get self.closed then raise Closed;

    let tr = Trigger.create () in
    if
      A.compare_and_set self.st old_st
        { old_st with pop_waiters = Q.push old_st.pop_waiters tr }
    then (
      Trigger.await_exn tr;
      pop self
    ) else
      pop self
  | x, new_st, to_broadcast ->
    if A.compare_and_set self.st old_st new_st then (
      Q.iter Trigger.signal to_broadcast;
      x
    ) else
      pop self

let push (self : _ t) x : unit =
  while
    if A.get self.closed then raise Closed;

    let old_st = A.get self.st in
    match State.push_one_ ~max_size:self.max_size old_st x with
    | exception Full ->
      let tr = Trigger.create () in
      if
        A.compare_and_set self.st old_st
          { old_st with push_waiters = Q.push old_st.push_waiters tr }
      then
        Trigger.await_exn tr;
      true
    | new_st, to_broadcast ->
      if A.compare_and_set self.st old_st new_st then (
        Q.iter Trigger.signal to_broadcast;
        false
      ) else
        true
  do
    Domain_.relax ()
  done

[@@@endif]
