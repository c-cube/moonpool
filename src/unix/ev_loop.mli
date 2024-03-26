(** Event loop *)

val wait_readable :
  Unix.file_descr -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val wait_writable :
  Unix.file_descr -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val run_after_s : float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
val run_every_s : float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val with_loop : runner:Moonpool.Runner.t -> (unit -> 'a) -> 'a
(** Run with the event loop processed in the current thread. There can
   only be one such loop running at a time.
   @raise Failure if another call to {!with_loop} is already in effect. *)
