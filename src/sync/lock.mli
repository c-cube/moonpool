(** Mutex-protected resource.

    This lock is a synchronous concurrency primitive, as a thin wrapper
    around {!Mutex} that encourages proper management of the critical
    section in RAII style:

    {[
      let (let@) = (@@)


      …
      let compute_foo =
        (* enter critical section *)
        let@ x = Lock.with_ protected_resource in
        use_x;
        return_foo ()
        (* exit critical section *)
      in
      …
    ]}

    This lock is based on {!Picos_sync.Mutex} so it is [await]-safe.

    @since NEXT_RELEASE *)

type 'a t
(** A value protected by a cooperative mutex *)

val create : 'a -> 'a t
(** Create a new protected value. *)

val with_ : 'a t -> ('a -> 'b) -> 'b
(** [with_ l f] runs [f x] where [x] is the value protected with
    the lock [l], in a critical section. If [f x] fails, [with_lock l f]
    fails too but the lock is released. *)

val update : 'a t -> ('a -> 'a) -> unit
(** [update l f] replaces the content [x] of [l] with [f x], while protected
    by the mutex. *)

val update_map : 'a t -> ('a -> 'a * 'b) -> 'b
(** [update_map l f] computes [x', y = f (get l)], then puts [x'] in [l]
    and returns [y], while protected by the mutex. *)

val mutex : _ t -> Picos_sync.Mutex.t
(** Underlying mutex. *)

val get : 'a t -> 'a
(** Atomically get the value in the lock. The value that is returned
    isn't protected! *)

val set : 'a t -> 'a -> unit
(** Atomically set the value.

    {b NOTE} caution: using {!get} and {!set} as if this were a {!ref}
    is an anti pattern and will not protect data against some race conditions. *)
