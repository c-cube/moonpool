(** Static pool of domains.

   These domains are shared between {b all} the pools in moonpool.
   The rationale is that we should not have more domains than cores, so
   it's easier to reserve exactly that many domain slots, and run more flexible
   thread pools on top (each domain being shared by potentially multiple threads
   from multiple pools).

   The pool should not contain actual domains if it's not in use, ie if no
   runner is presently actively using one or more of the domain slots.

   {b NOTE}: Interface is still experimental.

   @since NEXT_RELEASE
*)

type domain = Domain_.t

val max_number_of_domains : unit -> int
(** Number of domains in the pool when all domains are active. *)

(** {2 Low level interface for resouce handling}

    Be very cautious with this interface, or resource leaks might occur. *)

val run_on : int -> (unit -> unit) -> unit
(** [run_on i f] runs [f()] on the domain with index [i].
    Precondition: [0 <= i < n_domains()]. The thread must call {!decr_on}
    with [i] once it's done. *)

val decr_on : int -> unit
(** Signal that a thread is stopping on the domain with index [i]. *)

val run_on_and_wait : int -> (unit -> 'a) -> 'a
(** [run_on_and_wait i f] runs [f()] on the domain with index [i],
    and blocks until the result of [f()] is returned back. *)
