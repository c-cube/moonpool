(** Basic Blocking Queue *)

type 'a t

val create : unit -> _ t

exception Closed

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] into [q], and returns [()].
    @raise Closed if [close q] was previously called.*)

val size : _ t -> int
(** Number of items currently in the queue.
    @since 0.2 *)

val pop : 'a t -> 'a
(** [pop q] pops the next element in [q]. It might block until an element comes.
    @raise Closed if the queue was closed before a new element was available. *)

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

val close : _ t -> unit
(** Close the queue, meaning there won't be any more [push] allowed. *)

type 'a gen = unit -> 'a option
type 'a iter = ('a -> unit) -> unit

val to_iter : 'a t -> 'a iter
(** [to_iter q] returns an iterator over all items in the queue. This might not
    terminate if [q] is never closed.
    @since 0.4 *)

val to_gen : 'a t -> 'a gen
(** [to_gen q] returns a generator from the queue.
    @since 0.4 *)

val to_seq : 'a t -> 'a Seq.t
(** [to_gen q] returns a (transient) sequence from the queue.
    @since 0.4 *)
