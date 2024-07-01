module Slice = Iostream_types.Slice

val read : Fd.t -> bytes -> int -> int -> int
(** Non blocking read *)

val write_once : Fd.t -> bytes -> int -> int -> int
(** Non blocking write *)

val write : Fd.t -> bytes -> int -> int -> unit

module Reader : sig
  include module type of struct
    include Iostream_types.In
  end

  val of_fd : ?close_noerr:bool -> Fd.t -> t
  val of_slice : Slice.t -> t
end

module Buf_reader : sig
  include module type of struct
    include Iostream_types.In_buf
  end

  val of_fd : ?close_noerr:bool -> buf:bytes -> Fd.t -> t
end

module Writer : sig
  include module type of struct
    include Iostream_types.Out
  end

  val of_fd : ?close_noerr:bool -> Fd.t -> t
end

module Buf_writer : sig
  include module type of struct
    include Iostream_types.Out_buf
  end

  val of_fd : ?close_noerr:bool -> buf:bytes -> Fd.t -> t
end

module Buf_pool : sig
  type t = { with_buf: 'a. int -> (bytes -> 'a) -> 'a } [@@unboxed]

  val dummy : t
  (** Just allocate on the fly, no pooling *)
end

module TCP_client : sig
  val with_connect' : Sockaddr.t -> (Fd.t -> 'a) -> 'a

  val with_connect :
    ?buf_pool:Buf_pool.t ->
    ?buf_size:int ->
    Sockaddr.t ->
    (Buf_reader.t -> Buf_writer.t -> 'a) ->
    'a
end

module TCP_server : sig
  type conn_handler =
    client_addr:Sockaddr.t -> Buf_reader.t -> Buf_writer.t -> unit
  (** Handle client connection *)

  (** A running TCP server.

     This contains some functions that provide information about the running
     server, including whether it's active (as opposed to stopped), a function
     to stop it, and statistics about the number of connections. *)
  class type t = object
    method endpoint : unit -> Sockaddr.t
    (** Endpoint we listen on. This can only be called from within [serve]. *)

    method active_connections : unit -> int
    (** Number of connections currently active *)

    method running : unit -> bool
    (** Is the server currently running? *)

    method run : unit -> unit

    method stop : unit -> unit
    (** Ask the server to stop. This might not take effect immediately,
      and is idempotent. After this [server.running()] must return [false]. *)

    method await : unit -> unit
    (** Wait for the server to stop running *)
  end

  val stop : #t -> unit
  val run : #t -> unit
  val endpoint : #t -> Sockaddr.t
  val await : #t -> unit

  class base_server :
    ?listen:int ->
    ?buf_pool:Buf_pool.t ->
    ?buf_size:int ->
    runner:Moonpool.Runner.t ->
    handle:conn_handler ->
    Sockaddr.t ->
    t

  val create :
    ?after_init:(t -> unit) ->
    ?listen:int ->
    ?buf_pool:Buf_pool.t ->
    ?buf_size:int ->
    runner:Moonpool.Runner.t ->
    handle:conn_handler ->
    Sockaddr.t ->
    t

  val with_server :
    ?listen:int ->
    ?buf_pool:Buf_pool.t ->
    ?buf_size:int ->
    runner:Moonpool.Runner.t ->
    handle:conn_handler ->
    Sockaddr.t ->
    (t -> 'a) ->
    'a
end
