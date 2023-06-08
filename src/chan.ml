module A = Atomic_

type 'a or_error = 'a Fut.or_error
type 'a waiter = 'a Fut.promise

let[@inline] list_is_empty_ = function
  | [] -> true
  | _ :: _ -> false

(** Simple functional queue *)
module Q : sig
  type 'a t

  val return : 'a -> 'a t
  val is_empty : _ t -> bool

  exception Empty

  val pop_exn : 'a t -> 'a * 'a t
  val push : 'a t -> 'a -> 'a t
end = struct
  type 'a t = {
    hd: 'a list;
    tl: 'a list;
  }
  (** Queue containing elements of type 'a.

      invariant: if hd=[], then tl=[] *)

  let[@inline] return x : _ t = { hd = [ x ]; tl = [] }

  let make_ hd tl =
    match hd with
    | [] -> { hd = List.rev tl; tl = [] }
    | _ :: _ -> { hd; tl }

  let[@inline] is_empty q = list_is_empty_ q.hd
  let[@inline] push q x : _ t = make_ q.hd (x :: q.tl)

  exception Empty

  let pop_exn q =
    match q.hd with
    | [] ->
      assert (list_is_empty_ q.tl);
      raise Empty
    | x :: hd' ->
      let q' = make_ hd' q.tl in
      x, q'
end

exception Closed

type 'a state =
  | Empty
  | St_closed
  | Elems of 'a Q.t
  | Waiters of 'a waiter list

type 'a t = { st: 'a state A.t } [@@unboxed]

let create () : _ t = { st = A.make Empty }

let[@inline] mk_st_waiters_ l =
  match l with
  | [] -> Empty
  | _ -> Waiters l

let[@inline] mk_st_elems_ q =
  if Q.is_empty q then
    Empty
  else
    Elems q

let push (self : _ t) x : unit =
  while
    let old_st = A.get self.st in
    match old_st with
    | St_closed -> raise Closed
    | Empty -> not (A.compare_and_set self.st old_st (Elems (Q.return x)))
    | Waiters [] -> assert false
    | Waiters (w :: waiters') ->
      let new_st = mk_st_waiters_ waiters' in
      if A.compare_and_set self.st old_st new_st then (
        Fut.fulfill w (Ok x);
        false
      ) else
        true
    | Elems q -> not (A.compare_and_set self.st old_st (Elems (Q.push q x)))
  do
    Domain_.relax ()
  done

let try_pop (type elt) self : elt option =
  let module M = struct
    exception Found of elt
  end in
  try
    for _i = 1 to 10 do
      let old_st = A.get self.st in
      match old_st with
      | Elems q ->
        let x, q' = Q.pop_exn q in
        let new_st = mk_st_elems_ q' in
        if A.compare_and_set self.st old_st new_st then
          raise_notrace (M.Found x)
        else
          Domain_.relax ()
      | _ -> raise_notrace Exit
    done;
    None
  with
  | M.Found x -> Some x
  | Exit -> None

let pop (type elt) (self : _ t) : elt Fut.t =
  let module M = struct
    exception Ret of elt
    exception Fut of elt Fut.t
  end in
  try
    while
      let old_st = A.get self.st in
      (match old_st with
      | St_closed ->
        let bt = Printexc.get_callstack 10 in
        raise_notrace (M.Fut (Fut.fail Closed bt))
      | Elems q ->
        let x, q' = Q.pop_exn q in
        let new_st = mk_st_elems_ q' in
        if A.compare_and_set self.st old_st new_st then raise_notrace (M.Ret x)
      | Empty ->
        let fut, promise = Fut.make () in
        let new_st = Waiters [ promise ] in
        if A.compare_and_set self.st old_st new_st then
          raise_notrace (M.Fut fut)
      | Waiters ws ->
        let fut, promise = Fut.make () in
        let new_st = Waiters (promise :: ws) in
        if A.compare_and_set self.st old_st new_st then
          raise_notrace (M.Fut fut));
      true
    do
      Domain_.relax ()
    done;
    assert false
  with
  | M.Ret x -> Fut.return x
  | M.Fut f -> f

let pop_block_exn (self : 'a t) : 'a =
  match try_pop self with
  | Some x -> x
  | None -> Fut.wait_block_exn @@ pop self

let close (self : _ t) : unit =
  while
    let old_st = A.get self.st in
    match old_st with
    | St_closed -> false
    | Elems _ | Empty -> not (A.compare_and_set self.st old_st St_closed)
    | Waiters ws ->
      if A.compare_and_set self.st old_st St_closed then (
        (* fail all waiters *)
        let bt = Printexc.get_callstack 10 in
        List.iter (fun w -> Fut.fulfill_idempotent w (Error (Closed, bt))) ws;
        false
      ) else
        true
  do
    Domain_.relax ()
  done
