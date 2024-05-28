module A = Atomic_
module Q = Moonpool_private.Fqueue

type 'a or_error = 'a Fut.or_error
type 'a waiter = 'a Fut.promise

exception Closed

type 'a state =
  | Empty
  | St_closed
  | Elems of 'a Q.t
  | Waiters of 'a waiter Q.t

type 'a t = { st: 'a state A.t } [@@unboxed]

let create () : _ t = { st = A.make Empty }

(** Produce a state from a queue of waiters *)
let[@inline] mk_st_waiters_ ws : _ state =
  if Q.is_empty ws then
    Empty
  else
    Waiters ws

(** Produce a state from a queue of elements *)
let[@inline] mk_st_elems_ q : _ state =
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
    | Waiters ws ->
      (* awake first waiter and give it [x] *)
      let w, ws' = Q.pop_exn ws in
      let new_st = mk_st_waiters_ ws' in
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
    (* a bit of spinning *)
    for _retry = 1 to 10 do
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
        let new_st = Waiters (Q.return promise) in
        if A.compare_and_set self.st old_st new_st then
          raise_notrace (M.Fut fut)
      | Waiters ws ->
        let fut, promise = Fut.make () in
        (* add new promise at the end of the queue of waiters *)
        let new_st = Waiters (Q.push ws promise) in
        if A.compare_and_set self.st old_st new_st then
          raise_notrace (M.Fut fut));
      true
    do
      Domain_.relax ()
    done;
    (* never reached *)
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
    | St_closed -> false (* exit *)
    | Elems _ | Empty -> not (A.compare_and_set self.st old_st St_closed)
    | Waiters ws ->
      if A.compare_and_set self.st old_st St_closed then (
        (* fail all waiters with [Closed]. *)
        let bt = Printexc.get_callstack 10 in
        Q.iter (fun w -> Fut.fulfill_idempotent w (Error (Closed, bt))) ws;
        false
      ) else
        true
  do
    Domain_.relax ()
  done

[@@@ifge 5.0]

let pop_await self =
  match try_pop self with
  | Some x -> x
  | None -> Fut.await @@ pop self

[@@@endif]
