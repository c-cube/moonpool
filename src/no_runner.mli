(** Runner that runs in the caller, not in the background. *)

include module type of Runner

val runner : t
(** The trivial runner that actually runs tasks at the calling point. *)
