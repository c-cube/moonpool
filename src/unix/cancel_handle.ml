(** Cancellation handle. *)

open Common_

type state =
  | Cancelled
  | Waiting of { waiters: (unit -> unit) list }

type t = { st: state A.t } [@@unboxed]

let create () : t = { st = A.make (Waiting { waiters = [] }) }
let create_with f : t = { st = A.make (Waiting { waiters = [ f ] }) }

let cancel (self : t) =
  while
    let old_st = A.get self.st in
    match old_st with
    | Cancelled -> false
    | Waiting { waiters } ->
      if A.compare_and_set self.st old_st Cancelled then (
        List.iter (fun f -> f ()) waiters;
        false
      ) else
        true
  do
    ()
  done

let on_cancel (self : t) f : unit =
  while
    let old_st = A.get self.st in
    match old_st with
    | Cancelled ->
      f ();
      false
    | Waiting { waiters = l } ->
      not (A.compare_and_set self.st old_st (Waiting { waiters = f :: l }))
  do
    ()
  done

let dummy = { st = A.make Cancelled }
