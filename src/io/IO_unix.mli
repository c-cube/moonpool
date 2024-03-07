(** Low level Unix IOs *)

type file_descr = Unix.file_descr

val read : file_descr -> bytes -> int -> int -> int
val write_once : file_descr -> bytes -> int -> int -> int
val write : file_descr -> bytes -> int -> int -> unit
val await_readable : file_descr -> unit
val await_writable : file_descr -> unit

module In : sig
  include module type of Iostream.In

  val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> file_descr -> t
end

module Out : sig
  include module type of Iostream.Out

  val of_unix_fd : ?close_noerr:bool -> ?buf:bytes -> file_descr -> t
end
