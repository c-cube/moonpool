open Common_
include Base
module IO = IO
module IO_out = IO_out
module IO_in = IO_in

module TCP_server = struct
  type t = Lwt_io.server

  let establish ?backlog ?no_close ~runner addr handler : t =
    let server =
      Lwt_io.establish_server_with_client_socket ?backlog ?no_close addr
        (fun client_addr client_sock ->
          let ic = IO_in.of_unix_fd @@ Lwt_unix.unix_file_descr client_sock in
          let oc = IO_out.of_unix_fd @@ Lwt_unix.unix_file_descr client_sock in

          let fut =
            M.Fut.spawn ~name:"tcp.server.handler" ~on:runner (fun () ->
                handler client_addr ic oc)
          in

          let lwt_fut = lwt_of_fut fut in
          lwt_fut)
    in
    Printf.printf "awaiting server\n%!";
    let s = await_lwt server in
    Printf.printf "got server\n%!";
    s

  let shutdown self = await_lwt @@ Lwt_io.shutdown_server self
end

module TCP_client = struct
  let with_connect addr (f : IO_in.t -> IO_out.t -> 'a) : 'a =
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
        Moonpool.Private.Suspend_.suspend
          {
            handle =
              (fun ~run:_ ~resume sus ->
                Perform_action_in_lwt.schedule
                @@ Action.Wait_writable
                     ( sock,
                       fun ev ->
                         resume sus @@ Ok ();
                         Lwt_engine.stop_event ev ));
          };
        true
    do
      ()
    done;

    let ic = IO_in.of_unix_fd sock in
    let oc = IO_out.of_unix_fd sock in

    let finally () = try Unix.close sock with _ -> () in
    let@ () = Fun.protect ~finally in
    f ic oc
end
