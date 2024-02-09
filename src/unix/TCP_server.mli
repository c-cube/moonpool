type t

val port : t -> int

val with_server :
  ?addr:Unix.inet_addr ->
  ?port:int ->
  ?after_init:(t -> unit) ->
  runner:Moonpool.Runner.t ->
  handle_client:(t -> Unix.sockaddr -> IO_in.t -> IO_out.t -> unit) ->
  unit ->
  unit
