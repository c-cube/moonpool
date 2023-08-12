(** Static pool of domains.

   These domains are shared between {b all} the pools in moonpool.
   The rationale is that we should not have more domains than cores, so
   it's easier to pre-allocate exactly that many domains, and run more flexible
   thread pools on top.
*)

type domain = Domain_.t

val n_domains : unit -> int
(** Number of domains in the pool *)

val run_on : int -> (unit -> unit) -> unit
(** [run_on i f] runs [f()] on the domain with index [i].
    Precondition: [0 <= i < n_domains()] *)

val decr_on : int -> unit
(** Signal that a thread is stopping on the domain with index [i] *)

val run_on_and_wait : int -> (unit -> 'a) -> 'a
(** [run_on_and_wait i f] runs [f()] on the domain with index [i],
    and blocks until the result of [f()] is returned back. *)
