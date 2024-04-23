module A = Atomic_
module Q = Moonpool_private.Fqueue

type waiter = unit -> unit

type 'a state = {
  value: 'a;
  locked: bool;
  waiters: waiter Q.t;
}

type 'a t = { st: 'a state A.t } [@@unboxed]

let create x : _ t =
  { st = A.make { locked = false; value = x; waiters = Q.empty } }

let try_lock_ (self : 'a t) : 'a option =
  let old = A.get self.st in
  if old.locked then
    None
  else if A.compare_and_set self.st old { old with locked = true } then
    Some old.value
  else
    None

let rec lock_ (self : 'a t) : 'a =
  let old = A.get self.st in
  if old.locked then (
    (* suspend and add myself to the [waiters] queue *)
    Moonpool.Private.Suspend_.suspend
      {
        handle =
          (fun ~run:_ ~resume k ->
            let waiter : waiter = fun () -> resume k @@ Ok () in
            if
              A.compare_and_set self.st old
                { old with waiters = Q.push old.waiters waiter }
            then
              ()
            else
              (* wakeup now to try again *)
              resume k @@ Ok ());
      };
    lock_ self
  ) else if A.compare_and_set self.st old { old with locked = true } then
    old.value
  else
    lock_ self

let unlock_ (self : _ t) (x : 'a) : unit =
  let waiter_to_awake = ref None in
  while
    let old = A.get self.st in
    assert old.locked;
    (* check if there's a waiter to wake up *)
    let waiters =
      match Q.pop_exn old.waiters with
      | exception Q.Empty ->
        waiter_to_awake := None;
        old.waiters
      | w, ws ->
        waiter_to_awake := Some w;
        ws
    in
    let new_st = { locked = false; value = x; waiters } in
    not (A.compare_and_set self.st old new_st)
  do
    ()
  done;
  (* wakeup the next waiter, if any *)
  Option.iter (fun f -> f ()) !waiter_to_awake

let with_lock self f =
  let x = lock_ self in
  match f x with
  | res ->
    unlock_ self x;
    res
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    unlock_ self x;
    Printexc.raise_with_backtrace e bt

let with_lock_update self f =
  let x = lock_ self in
  match f x with
  | new_x, res ->
    unlock_ self new_x;
    res
  | exception e ->
    let bt = Printexc.get_raw_backtrace () in
    unlock_ self x;
    Printexc.raise_with_backtrace e bt

let with_try_lock self f =
  match try_lock_ self with
  | None -> f None
  | Some x ->
    (match f (Some x) with
    | res ->
      unlock_ self x;
      res
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      unlock_ self x;
      Printexc.raise_with_backtrace e bt)
