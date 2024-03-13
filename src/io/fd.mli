type t
(** A wrapper around a unix file descriptor.
    This makes closing idempotent and ensures that the
    Unix FD is only accessible while this is open. *)

exception Closed

val create : ?close_noerr:bool -> Unix.file_descr -> t
(** Create a new file descriptor from a unix one. *)

val fd : t -> Unix.file_descr
(** Access the underlying file descriptor.
    @raise Closed if the FD was closed. *)

val close : t -> unit
(** Close the FD. Idempotent. *)

val read : t -> bytes -> int -> int -> int
(** Read from the FD. Suspends if the FD is not ready.
    @return 0 if the FD is closed *)

val write_once : t -> bytes -> int -> int -> int
(** Write into the FD. Suspends if the FD is not ready.
    @return 0 if the FD is closed *)

val write : t -> bytes -> int -> int -> unit
(** A loop around {!write_once}.
    @raise Closed if the FD is closed before this completes. *)

val await_readable : t -> unit
(** Wait for the FD to be readable.
    @raise Closed if the FD is closed. *)

val await_writable : t -> unit
(** Wait for the FD to be writable.
    @raise Closed if the FD is closed. *)

class to_in : t -> Iostream.In.t
class to_out : t -> Iostream.Out.t
class to_in_buf : ?bytes:bytes -> t -> Iostream.In_buf.t
class to_out_buf : ?bytes:bytes -> t -> Iostream.Out_buf.t
