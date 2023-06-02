(** Moonpool

  A pool within a bigger pool (ie the ocean). Here, we're talking about
  pools of [Thread.t] which live within a fixed pool of [Domain.t].
*)

type 'a or_error = ('a, exn * Printexc.raw_backtrace) result

module Pool = Pool

val start_thread_on_some_domain : ('a -> unit) -> 'a -> Thread.t
(** Similar to {!Thread.create}, but it picks a background domain at random
    to run the thread. This ensures that we don't always pick the same domain
    to run all the various threads needed in an application (timers, event loops, etc.) *)

module Fut = Fut
