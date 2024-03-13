open Common_

module Inet_addr = struct
  type t = Unix.inet_addr

  let any = Unix.inet_addr_any
  let loopback = Unix.inet_addr_loopback
  let show = Unix.string_of_inet_addr
  let of_string s = try Some (Unix.inet_addr_of_string s) with _ -> None

  let of_string_exn s =
    try Unix.inet_addr_of_string s with _ -> invalid_arg "Inet_addr.of_string"
end

module Sockaddr = struct
  type t = Unix.sockaddr

  let show = function
    | Unix.ADDR_UNIX s -> s
    | Unix.ADDR_INET (addr, port) ->
      spf "%s:%d" (Unix.string_of_inet_addr addr) port

  let unix s : t = Unix.ADDR_UNIX s
  let inet addr port : t = Unix.ADDR_INET (addr, port)

  let inet_parse addr port =
    try Some (inet (Unix.inet_addr_of_string addr) port) with _ -> None

  let inet_parse_exn addr port =
    try inet (Unix.inet_addr_of_string addr) port
    with _ -> invalid_arg "Sockadd.inet_parse"

  let inet_local port = inet Unix.inet_addr_loopback port
  let inet_any port = inet Unix.inet_addr_any port
end

module TCP_server = struct
  type t = { fut: unit Fut.t } [@@unboxed]

  exception Stop

  let stop_ fut =
    let ebt = Exn_bt.get_callstack 20 Stop in
    let promise = Fut.Private_.unsafe_promise_of_fut fut in
    Fut.fulfill_idempotent promise (Error ebt)

  let stop self = stop_ self.fut
  let join self = Fut.await self.fut

  let with_serve' (addr : Sockaddr.t) handle_client (f : t -> 'a) : 'a =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    Unix.bind sock addr;
    Unix.set_nonblock sock;
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.listen sock 32;
    let fd_sock = Fd.create sock in

    let fut, promise = Fut.make () in
    let self = { fut } in

    let loop_client client_sock client_addr : unit =
      Unix.setsockopt client_sock Unix.TCP_NODELAY true;
      let client_sock = Fd.create ~close_noerr:true client_sock in

      let@ () = Fun.protect ~finally:(fun () -> Fd.close client_sock) in
      handle_client client_addr client_sock
    in

    let loop () =
      while not (Fut.is_done fut) do
        match Unix.accept sock with
        | client_sock, client_addr ->
          ignore
            (Fiber.spawn ~protect:true (fun () ->
                 loop_client client_sock client_addr)
              : _ Fiber.t)
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          (* FIXME: possible race condition: the socket became readable
              in the mid-time and we won't get notified. We need to call
              [accept] after subscribing to [on_readable]. *)
          (* suspend *)
          Fd.await_readable fd_sock
      done
    in

    let _loop_fiber : unit Fiber.t = Fiber.spawn ~protect:false loop in
    let finally () =
      Fut.fulfill_idempotent promise @@ Ok ();
      Fd.close fd_sock
    in
    let@ () = Fun.protect ~finally in
    f self

  let with_serve (addr : Sockaddr.t) handle_client (f : t -> 'a) : 'a =
    with_serve' addr
      (fun client_addr (client_sock : Fd.t) ->
        let ic = new Fd.to_in_buf client_sock in
        let oc = new Fd.to_out_buf client_sock in
        handle_client client_addr ic oc)
      f
end

module TCP_client = struct
  let with_connect' addr (f : Fd.t -> 'a) : 'a =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt sock Unix.TCP_NODELAY true;
    let sock = Fd.create ~close_noerr:true sock in

    (* connect asynchronously *)
    while
      try
        let fd = Fd.fd sock in
        Unix.connect fd addr;
        false
      with
      | Unix.Unix_error
          ((Unix.EWOULDBLOCK | Unix.EINPROGRESS | Unix.EAGAIN), _, _)
      ->
        Fd.await_writable sock;
        true
    do
      ()
    done;

    let finally () = Fd.close sock in
    let@ () = Fun.protect ~finally in
    f sock

  let with_connect addr (f : Iostream.In_buf.t -> Iostream.Out_buf.t -> 'a) : 'a
      =
    with_connect' addr (fun sock ->
        let ic = new Fd.to_in_buf sock in
        let oc = new Fd.to_out_buf sock in
        f ic oc)
end
