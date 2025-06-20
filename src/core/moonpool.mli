(** Moonpool

    A pool within a bigger pool (ie the ocean). Here, we're talking about pools
    of [Thread.t] that are dispatched over several [Domain.t] to enable
    parallelism.

    We provide several implementations of pools with distinct scheduling
    strategies, alongside some concurrency primitives such as guarding locks
    ({!Lock.t}) and futures ({!Fut.t}). *)

module Ws_pool = Ws_pool
module Fifo_pool = Fifo_pool
module Background_thread = Background_thread
module Runner = Runner
module Trigger = Trigger

module Immediate_runner : sig end
[@@deprecated "use Moonpool_fib.Main"]
(** Runner that runs tasks in the caller thread.

    This is removed since 0.6, and replaced by {!Moonpool_fib.Main}. *)

module Exn_bt = Exn_bt

exception Shutdown
(** Exception raised when trying to run tasks on runners that have been shut
    down.
    @since 0.6 *)

val start_thread_on_some_domain : ('a -> unit) -> 'a -> Thread.t
(** Similar to {!Thread.create}, but it picks a background domain at random to
    run the thread. This ensures that we don't always pick the same domain to
    run all the various threads needed in an application (timers, event loops,
    etc.) *)

val run_async : ?fiber:Picos.Fiber.t -> Runner.t -> (unit -> unit) -> unit
(** [run_async runner task] schedules the task to run on the given runner. This
    means [task()] will be executed at some point in the future, possibly in
    another thread.
    @param fiber optional initial (picos) fiber state
    @since 0.5 *)

val run_wait_block : ?fiber:Picos.Fiber.t -> Runner.t -> (unit -> 'a) -> 'a
(** [run_wait_block runner f] schedules [f] for later execution on the runner,
    like {!run_async}. It then blocks the current thread until [f()] is done
    executing, and returns its result. If [f()] raises an exception, then
    [run_wait_block pool f] will raise it as well.

    See {!run_async} for more details.

    {b NOTE} be careful with deadlocks (see notes in {!Fut.wait_block} about the
    required discipline to avoid deadlocks).
    @raise Shutdown if the runner was already shut down
    @since 0.6 *)

val recommended_thread_count : unit -> int
(** Number of threads recommended to saturate the CPU. For IO pools this makes
    little sense (you might want more threads than this because many of them
    will be blocked most of the time).
    @since 0.5 *)

val spawn : on:Runner.t -> (unit -> 'a) -> 'a Fut.t
(** [spawn ~on f] runs [f()] on the runner (a thread pool typically) and returns
    a future result for it. See {!Fut.spawn}.
    @since 0.5 *)

val spawn_on_current_runner : (unit -> 'a) -> 'a Fut.t
(** See {!Fut.spawn_on_current_runner}.
    @since 0.5 *)

val get_current_runner : unit -> Runner.t option
(** See {!Runner.get_current_runner}
    @since 0.7 *)

[@@@ifge 5.0]

val await : 'a Fut.t -> 'a
(** Await a future, must be run on a moonpool runner. See {!Fut.await}. Only on
    OCaml >= 5.0.
    @since 0.5 *)

val yield : unit -> unit
(** Yield from the current task, must be run on a moonpool runner. Only on OCaml
    >= 5.0.
    @since NEXT_RELEASE *)

[@@@endif]

module Lock = Lock
module Fut = Fut
module Chan = Chan
module Task_local_storage = Task_local_storage
module Thread_local_storage = Thread_local_storage

(** A simple blocking queue.

    This queue is quite basic and will not behave well under heavy contention.
    However, it can be sufficient for many practical use cases.

    {b NOTE}: this queue will typically block the caller thread in case the
    operation (push/pop) cannot proceed. Be wary of deadlocks when using the
    queue {i from} a pool when you expect the other end to also be
    produced/consumed from the same pool.

    See discussion on {!Fut.wait_block} for more details on deadlocks and how to
    mitigate the risk of running into them.

    More scalable queues can be found in Lockfree
    (https://github.com/ocaml-multicore/lockfree/) *)
module Blocking_queue : sig
  type 'a t
  (** Unbounded blocking queue.

      This queue is thread-safe and will block when calling {!pop} on it when
      it's empty. *)

  val create : unit -> _ t
  (** Create a new unbounded queue. *)

  val size : _ t -> int
  (** Number of items currently in the queue. Note that [pop] might still block
      if this returns a non-zero number, since another thread might have
      consumed the items in the mean time.
      @since 0.2 *)

  exception Closed

  val push : 'a t -> 'a -> unit
  (** [push q x] pushes [x] into [q], and returns [()].

      In the current implementation, [push q] will never block for a long time,
      it will only block while waiting for a lock so it can push the element.
      @raise Closed if the queue is closed (by a previous call to [close q]) *)

  val pop : 'a t -> 'a
  (** [pop q] pops the next element in [q]. It might block until an element
      comes.
      @raise Closed if the queue was closed before a new element was available.
  *)

  val close : _ t -> unit
  (** Close the queue, meaning there won't be any more [push] allowed, ie [push]
      will raise {!Closed}.

      [pop] will keep working and will return the elements present in the queue,
      until it's entirely drained; then [pop] will also raise {!Closed}. *)

  val try_pop : force_lock:bool -> 'a t -> 'a option
  (** [try_pop q] immediately pops the first element of [q], if any, or returns
      [None] without blocking.
      @param force_lock
        if true, use {!Mutex.lock} (which can block under contention); if false,
        use {!Mutex.try_lock}, which might return [None] even in presence of an
        element if there's contention *)

  val try_push : 'a t -> 'a -> bool
  (** [try_push q x] tries to push into [q], in which case it returns [true]; or
      it fails to push and returns [false] without blocking.
      @raise Closed if the locking succeeded but the queue is closed. *)

  val transfer : 'a t -> 'a Queue.t -> unit
  (** [transfer bq q2] transfers all items presently in [bq] into [q2] in one
      atomic section, and clears [bq]. It blocks if no element is in [bq].

      This is useful to consume elements from the queue in batch. Create a
      [Queue.t] locally:

      {[
        let dowork (work_queue : job Bb_queue.t) =
          (* local queue, not thread safe *)
          let local_q = Queue.create () in
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
  (** [to_iter q] returns an iterator over all items in the queue. This might
      not terminate if [q] is never closed.
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

    This is either a shim using [ref], on pre-OCaml 5, or the standard [Atomic]
    module on OCaml 5. *)

(**/**)

(** Private internals, with no stability guarantees *)
module Private : sig
  module Ws_deque_ = Ws_deque_
  (** A deque for work stealing, fixed size. *)

  module Worker_loop_ = Worker_loop_
  (** Worker loop. This is useful to implement custom runners, it should run on
      each thread of the runner.
      @since 0.7 *)

  module Domain_ = Domain_
  (** Utils for domains *)

  module Tracing_ = Tracing_
  module Types_ = Types_

  val num_domains : unit -> int
  (** Number of domains in the backing domain pool *)
end
