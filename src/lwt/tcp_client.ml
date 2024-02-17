open Common_
open Base

let connect addr : Unix.file_descr =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.set_nonblock sock;
  Unix.setsockopt sock Unix.TCP_NODELAY true;

  (* connect asynchronously *)
  while
    try
      Unix.connect sock addr;
      false
    with
    | Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EINPROGRESS | Unix.EAGAIN), _, _)
    ->
      IO.await_writable sock;
      true
  do
    ()
  done;
  sock

let with_connect addr (f : IO_in.t -> IO_out.t -> 'a) : 'a =
  let sock = connect addr in

  let ic = IO_in.of_unix_fd sock in
  let oc = IO_out.of_unix_fd sock in

  let finally () = try Unix.close sock with _ -> () in
  let@ () = Fun.protect ~finally in
  f ic oc

let with_connect_lwt addr
    (f : Lwt_io.input_channel -> Lwt_io.output_channel -> 'a) : 'a =
  let sock = connect addr in

  let ic =
    run_in_lwt_and_await (fun () ->
        Lwt.return @@ Lwt_io.of_unix_fd ~mode:Lwt_io.input sock)
  in
  let oc =
    run_in_lwt_and_await (fun () ->
        Lwt.return @@ Lwt_io.of_unix_fd ~mode:Lwt_io.output sock)
  in

  let finally () =
    (try run_in_lwt_and_await (fun () -> Lwt_io.close ic) with _ -> ());
    (try run_in_lwt_and_await (fun () -> Lwt_io.close oc) with _ -> ());
    try Unix.close sock with _ -> ()
  in
  let@ () = Fun.protect ~finally in
  f ic oc
