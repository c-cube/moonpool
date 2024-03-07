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
  type t = { fiber: unit Fiber.t } [@@unboxed]

  exception Stop

  let stop_ fiber =
    let ebt = Exn_bt.get Stop in
    Fuseau.Fiber.Private_.cancel fiber ebt

  let stop self = stop_ self.fiber
  let join self = Fuseau.await self.fiber

  let with_serve' (addr : Sockaddr.t) handle_client (f : t -> 'a) : 'a =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    Unix.bind sock addr;
    Unix.set_nonblock sock;
    Unix.setsockopt sock Unix.SO_REUSEADDR true;
    Unix.listen sock 32;

    let fiber = Fuseau.Fiber.Private_.create () in
    let self = { fiber } in

    let loop_client client_sock client_addr : unit =
      Unix.set_nonblock client_sock;
      Unix.setsockopt client_sock Unix.TCP_NODELAY true;

      let@ () =
        Fun.protect ~finally:(fun () ->
            try Unix.close client_sock with _ -> ())
      in
      handle_client client_addr client_sock
    in

    let loop () =
      while not (Fiber.is_done fiber) do
        match Unix.accept sock with
        | client_sock, client_addr ->
          ignore
            (Fuseau.spawn ~propagate_cancel_to_parent:false (fun () ->
                 loop_client client_sock client_addr)
              : _ Fiber.t)
        | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          (* suspend *)
          let loop = U_loop.cur () in
          Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
              (* FIXME: possible race condition: the socket became readable
                  in the mid-time and we won't get notified. We need to call
                  [accept] after subscribing to [on_readable]. *)
              ignore
                (loop#on_readable sock (fun _ev ->
                     wakeup ();
                     Cancel_handle.cancel _ev)
                  : Cancel_handle.t))
      done
    in

    let loop_fiber =
      let sched = Fuseau.get_scheduler () in
      Fuseau.spawn_as_child_of ~propagate_cancel_to_parent:true sched fiber loop
    in
    let finally () =
      stop_ loop_fiber;
      Unix.close sock
    in
    let@ () = Fun.protect ~finally in
    f self

  let with_serve (addr : Sockaddr.t) handle_client (f : t -> 'a) : 'a =
    with_serve' addr
      (fun client_addr client_sock ->
        let ic = IO_unix.In.of_unix_fd client_sock in
        let oc = IO_unix.Out.of_unix_fd client_sock in
        handle_client client_addr ic oc)
      f
end

module TCP_client = struct
  let with_connect' addr (f : Unix.file_descr -> 'a) : 'a =
    let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.set_nonblock sock;
    Unix.setsockopt sock Unix.TCP_NODELAY true;

    (* connect asynchronously *)
    while
      try
        Unix.connect sock addr;
        false
      with
      | Unix.Unix_error
          ((Unix.EWOULDBLOCK | Unix.EINPROGRESS | Unix.EAGAIN), _, _)
      ->
        Fuseau.Private_.suspend ~before_suspend:(fun ~wakeup ->
            let loop = U_loop.cur () in
            ignore
              (loop#on_writable sock (fun _ev ->
                   wakeup ();
                   Cancel_handle.cancel _ev)
                : Cancel_handle.t));
        true
    do
      ()
    done;

    let finally () = try Unix.close sock with _ -> () in
    let@ () = Fun.protect ~finally in
    f sock

  let with_connect addr (f : Iostream.In.t -> Iostream.Out.t -> 'a) : 'a =
    with_connect' addr (fun sock ->
        let ic = IO_unix.In.of_unix_fd sock in
        let oc = IO_unix.Out.of_unix_fd sock in
        f ic oc)
end
