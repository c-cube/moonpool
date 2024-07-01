(** Event loop *)

val wait_readable : Fd.t -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
val wait_writable : Fd.t -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
val run_after_s : float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
val run_every_s : float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit
