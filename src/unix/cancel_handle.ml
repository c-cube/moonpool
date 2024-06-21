(** Cancellation handle. *)

open Common_

type waiter = Waiter : ('a -> unit) * 'a -> waiter

type state =
  | Cancelled
  | Waiting of { waiters: waiter list }

type t = { st: state A.t } [@@unboxed]

let create () : t = { st = A.make (Waiting { waiters = [] }) }

let create_with f : t =
  { st = A.make (Waiting { waiters = [ Waiter (f, ()) ] }) }

let cancel (self : t) =
  while
    let old_st = A.get self.st in
    match old_st with
    | Cancelled -> false
    | Waiting { waiters } ->
      if A.compare_and_set self.st old_st Cancelled then (
        List.iter (fun (Waiter (f, x)) -> f x) waiters;
        false
      ) else
        true
  do
    ()
  done

let on_cancel1 (self : t) f x : unit =
  let waiter = Waiter (f, x) in
  while
    let old_st = A.get self.st in
    match old_st with
    | Cancelled ->
      f x;
      false
    | Waiting { waiters = l } ->
      not (A.compare_and_set self.st old_st (Waiting { waiters = waiter :: l }))
  do
    ()
  done

let[@inline] on_cancel self f = on_cancel1 self f ()
let dummy = { st = A.make Cancelled }
