open Common_
open Base

type t = Lwt_io.server

let establish_lwt ?backlog ?no_close ~runner addr handler : t =
  let server =
    Lwt_io.establish_server_with_client_socket ?backlog ?no_close addr
      (fun client_addr client_sock ->
        let ic = Lwt_io.of_fd ~mode:Lwt_io.input client_sock in
        let oc = Lwt_io.of_fd ~mode:Lwt_io.output client_sock in

        let fut =
          M.Fut.spawn ~on:runner (fun () -> handler client_addr ic oc)
        in

        let lwt_fut = lwt_of_fut fut in
        lwt_fut)
  in
  await_lwt server

let establish ?backlog ?no_close ~runner addr handler : t =
  let server =
    Lwt_io.establish_server_with_client_socket ?backlog ?no_close addr
      (fun client_addr client_sock ->
        let ic = IO_in.of_unix_fd @@ Lwt_unix.unix_file_descr client_sock in
        let oc = IO_out.of_unix_fd @@ Lwt_unix.unix_file_descr client_sock in

        let fut =
          M.Fut.spawn ~on:runner (fun () -> handler client_addr ic oc)
        in

        let lwt_fut = lwt_of_fut fut in
        lwt_fut)
  in
  await_lwt server

let shutdown self = await_lwt @@ Lwt_io.shutdown_server self
