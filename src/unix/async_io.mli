module Slice = Iostream.Slice

val read : Fd.t -> bytes -> int -> int -> int
(** Non blocking read *)

val write_once : Fd.t -> bytes -> int -> int -> int
(** Non blocking write *)

val write : Fd.t -> bytes -> int -> int -> unit

module Reader : sig
  include module type of struct
    include Iostream.In
  end

  val of_fd : ?close_noerr:bool -> Fd.t -> t
  val of_slice : Slice.t -> t
end

module Buf_reader : sig
  include module type of struct
    include Iostream.In_buf
  end

  val of_fd : ?close_noerr:bool -> buf:bytes -> Fd.t -> t
end

module Writer : sig
  include module type of struct
    include Iostream.Out
  end

  val of_fd : ?close_noerr:bool -> Fd.t -> t
end

module Buf_writer : sig
  include module type of struct
    include Iostream.Out_buf
  end

  val of_fd : ?close_noerr:bool -> buf:bytes -> Fd.t -> t
end
