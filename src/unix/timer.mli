type t

val create : unit -> t
(** A new timer. *)

type tick_res =
  | Wait of float  (** Wait for number of seconds *)
  | Run of (Cancel_handle.t -> unit) * Cancel_handle.t
  | Empty  (** Next action to take *)

val next : t -> tick_res
(** Next action to take *)

val run_after_s :
  t -> float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val run_every_s :
  t -> float -> Cancel_handle.t -> (Cancel_handle.t -> unit) -> unit

val has_tasks : t -> bool
(** Does the timer contain anything? *)
