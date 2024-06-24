(** Event loop *)

val wait_readable :
  Unix.file_descr -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val wait_writable :
  Unix.file_descr -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val run_after_s : float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
val run_every_s : float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
