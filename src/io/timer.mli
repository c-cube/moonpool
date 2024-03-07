(** A simple timer implementation *)

type t
(** A timer. Not thread-safe. *)

val create : unit -> t
(** A new timer. *)

type tick_res =
  | Wait of float
  | Run of (unit -> unit)
  | Empty

val next : t -> tick_res
val run_after_s : t -> float -> Handle.t -> (unit -> unit) -> unit
val run_every_s : t -> float -> Handle.t -> (unit -> unit) -> unit
val cancel : t -> Handle.t -> unit
val has_tasks : t -> bool
