(** A blocking queue of finite size.

    This queue, while still using locks underneath
    (like the regular blocking queue) should be enough for
    usage under reasonable contention.

    The bounded size is helpful whenever some form of backpressure is
    desirable: if the queue is used to communicate between producer(s)
    and consumer(s), the consumer(s) can limit the rate at which
    producer(s) send new work down their way.
    Whenever the queue is full, means that producer(s) will have to
    wait before pushing new work.

    @since NEXT_RELEASE *)

type 'a t
(** A bounded queue. *)

val create : max_size:int -> unit -> 'a t

val close : _ t -> unit
(** [close q] closes [q]. No new elements can be pushed into [q],
    and after all the elements still in [q] currently are [pop]'d,
    {!pop} will also raise {!Closed}. *)

exception Closed

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] at the end of the queue.
    If [q] is full, this will block until there is
    room for [x].
    @raise Closed if [q] is closed. *)

val try_push : 'a t -> 'a -> bool
(** [try_push q x] attempts to push [x] into [q], but abandons
    if it cannot acquire [q] or if [q] is full.
    @raise Closed if [q] is closed. *)

val pop : 'a t -> 'a
(** [pop q] pops the first element off [q]. It blocks if [q]
    is empty, until some element becomes available.
    @raise Closed if [q] is empty and closed. *)

val try_pop : force_lock:bool -> 'a t -> 'a option
(** [try_pop q] tries to pop the first element, or returns [None]
    if no element is available or if it failed to acquire [q].

    @param force_lock if true, use {!Mutex.lock} (which can block
      under contention);
      if false, use {!Mutex.try_lock}, which might return [None] even in
      presence of an element if there's contention.

    @raise Closed if [q] is empty and closed. *)

val size : _ t -> int
(** Number of elements currently in [q] *)

val max_size : _ t -> int
(** Maximum size of the queue. See {!create}. *)

val transfer : 'a t -> 'a Queue.t -> unit
(** [transfer bq q2] transfers all elements currently available
    in [bq] into local queue [q2], and clears [bq], atomically.
    It blocks if [bq] is empty.

    See {!Bb_queue.transfer} for more details.
    @raise Closed if [bq] is empty and closed. *)

type 'a gen = unit -> 'a option
type 'a iter = ('a -> unit) -> unit

val to_iter : 'a t -> 'a iter
(** [to_iter q] returns an iterator over all items in the queue.
    This might not terminate if [q] is never closed.
    @since NEXT_RELEASE *)

val to_gen : 'a t -> 'a gen
(** [to_gen q] returns a generator from the queue.
    @since NEXT_RELEASE *)

val to_seq : 'a t -> 'a Seq.t
(** [to_gen q] returns a (transient) sequence from the queue.
    @since NEXT_RELEASE *)
