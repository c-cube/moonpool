(** Moonpool

  A pool within a bigger pool (ie the ocean). Here, we're talking about
  pools of [Thread.t] that are dispatched over several [Domain.t] to
  enable parallelism.

  We provide several implementations of pools
  with distinct scheduling strategies, alongside some concurrency
  primitives such as guarding locks ({!Lock.t}) and futures ({!Fut.t}).
*)

module Pool = Pool
module Fifo_pool = Fifo_pool
module Runner = Runner

val start_thread_on_some_domain : ('a -> unit) -> 'a -> Thread.t
(** Similar to {!Thread.create}, but it picks a background domain at random
    to run the thread. This ensures that we don't always pick the same domain
    to run all the various threads needed in an application (timers, event loops, etc.) *)

module Lock = Lock
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

  val try_pop : force_lock:bool -> 'a t -> 'a option
  (** [try_pop q] immediately pops the first element of [q], if any,
      or returns [None] without blocking.
      @param force_lock if true, use {!Mutex.lock} (which can block under contention);
        if false, use {!Mutex.try_lock}, which might return [None] even in
      presence of an element if there's contention *)

  val try_push : 'a t -> 'a -> bool
  (** [try_push q x] tries to push into [q], in which case
      it returns [true]; or it fails to push and returns [false]
      without blocking.
      @raise Closed if the locking succeeded but the queue is closed.
  *)

  val transfer : 'a t -> 'a Queue.t -> unit
  (** [transfer bq q2] transfers all items presently
    in [bq] into [q2] in one atomic section, and clears [bq].
    It blocks if no element is in [bq].

    This is useful to consume elements from the queue in batch.
    Create a [Queue.t] locally:


    {[
    let dowork (work_queue: job Bb_queue.t) =
      (* local queue, not thread safe *)
      let local_q = Queue.create() in
      try
        while true do
          (* work on local events, already on this thread *)
          while not (Queue.is_empty local_q) do
            let job = Queue.pop local_q in
            process_job job
          done;

          (* get all the events in the incoming blocking queue, in
             one single critical section. *)
          Bb_queue.transfer work_queue local_q
        done
      with Bb_queue.Closed -> ()
    ]}

    @since 0.4 *)

  type 'a gen = unit -> 'a option
  type 'a iter = ('a -> unit) -> unit

  val to_iter : 'a t -> 'a iter
  (** [to_iter q] returns an iterator over all items in the queue.
      This might not terminate if [q] is never closed.
      @since 0.4 *)

  val to_gen : 'a t -> 'a gen
  (** [to_gen q] returns a generator from the queue.
      @since 0.4 *)

  val to_seq : 'a t -> 'a Seq.t
  (** [to_gen q] returns a (transient) sequence from the queue.
      @since 0.4 *)
end

module Bounded_queue = Bounded_queue

module Atomic = Atomic_
(** Atomic values.

    This is either a shim using [ref], on pre-OCaml 5, or the
    standard [Atomic] module on OCaml 5. *)

(**/**)

module Private : sig
  module Ws_deque_ = Ws_deque_

  (** {2 Suspensions} *)

  module Suspend_ = Suspend_
  [@@alert
    unstable "this module is an implementation detail of moonpool for now"]
  (** Suspensions.

    This is only going to work on OCaml 5.x.

    {b NOTE}: this is not stable for now. *)
end
