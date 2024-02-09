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
