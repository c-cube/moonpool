(** Moonpool

  A pool within a bigger pool (ie the ocean). Here, we're talking about
  pools of [Thread.t] which live within a fixed pool of [Domain.t].
*)

module Pool = Pool

val start_thread_on_some_domain : ('a -> unit) -> 'a -> Thread.t
(** Similar to {!Thread.create}, but it picks a background domain at random
    to run the thread. This ensures that we don't always pick the same domain
    to run all the various threads needed in an application (timers, event loops, etc.) *)

module Fut = Fut
module Chan = Chan
module Fork_join = Fork_join

(** A simple blocking queue.

    This queue is quite basic and will not behave well under heavy
    contention. However, it can be sufficient for many practical use cases.

    {b NOTE}: this queue will typically block the caller thread
    in case the operation (push/pop) cannot proceed.
    Be wary of deadlocks when using the queue {i from} a pool
    when you expect the other end to also be produced/consumed from
    the same pool.

    See discussion on {!Fut.wait_block} for more details on deadlocks
    and how to mitigate the risk of running into them.

    More scalable queues can be found in
    Lockfree (https://github.com/ocaml-multicore/lockfree/)
*)
module Blocking_queue : sig
  type 'a t
  (** Unbounded blocking queue.

      This queue is thread-safe and will block when calling {!pop}
      on it when it's empty. *)

  val create : unit -> _ t
  (** Create a new unbounded queue. *)

  val size : _ t -> int
  (** Number of items currently in the queue. Note that [pop]
      might still block if this returns a non-zero number, since another
      thread might have consumed the items in the mean time.
      @since 0.2 *)

  exception Closed

  val push : 'a t -> 'a -> unit
  (** [push q x] pushes [x] into [q], and returns [()].

      In the current implementation, [push q] will never block for
      a long time, it will only block while waiting for a lock
      so it can push the element.
      @raise Closed if the queue is closed (by a previous call to [close q]) *)

  val pop : 'a t -> 'a
  (** [pop q] pops the next element in [q]. It might block until an element comes.
      @raise Closed if the queue was closed before a new element was available. *)

  val close : _ t -> unit
  (** Close the queue, meaning there won't be any more [push] allowed,
      ie [push] will raise {!Closed}.

      [pop] will keep working and will return the elements present in the
      queue, until it's entirely drained; then [pop] will
      also raise {!Closed}. *)
end

module Atomic = Atomic_
(** Atomic values.

    This is either a shim using [ref], on pre-OCaml 5, or the
    standard [Atomic] module on OCaml 5. *)
