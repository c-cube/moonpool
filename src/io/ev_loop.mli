open Common_

type io_mode =
  | Read
  | Write

type t

val get_or_create : unit -> t
val on_readable : t -> Unix.file_descr -> (unit -> unit) -> Cancel_handle.t
val on_writable : t -> Unix.file_descr -> (unit -> unit) -> Cancel_handle.t
val run_after_s : t -> float -> (unit -> unit) -> Cancel_handle.t
val run_every_s : t -> float -> (Cancel_handle.t -> unit) -> Cancel_handle.t
