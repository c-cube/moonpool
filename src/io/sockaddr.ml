type t = Unix.sockaddr

let show = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let unix str : t = Unix.ADDR_UNIX str
let inet addr port : t = Unix.ADDR_INET (addr, port)
let localhost port : t = inet Unix.inet_addr_loopback port
let any port : t = inet Unix.inet_addr_any port
let pp out (self : t) = Format.pp_print_string out (show self)

let domain = function
  | Unix.ADDR_UNIX _ -> Unix.PF_UNIX
  | Unix.ADDR_INET (a, _) ->
    if Unix.is_inet6_addr a then
      Unix.PF_INET6
    else
      Unix.PF_INET
