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
  sock

let with_connect addr (f : IO_in.t -> IO_out.t -> 'a) : 'a =
  let sock = connect addr in

  let ic = IO_in.of_unix_fd sock in
  let oc = IO_out.of_unix_fd sock in

  let finally () = try Unix.close sock with _ -> () in
  let@ () = Fun.protect ~finally in
  f ic oc

let with_connect' addr (f : Lwt_io.input_channel -> Lwt_io.output_channel -> 'a)
    : 'a =
  let sock = connect addr in

  let ic = Lwt_io.of_unix_fd ~mode:Lwt_io.input sock in
  let oc = Lwt_io.of_unix_fd ~mode:Lwt_io.output sock in

  let finally () =
    (try Lwt_io.close ic |> await_lwt with _ -> ());
    (try Lwt_io.close oc |> await_lwt with _ -> ());
    try Unix.close sock with _ -> ()
  in
  let@ () = Fun.protect ~finally in
  f ic oc
