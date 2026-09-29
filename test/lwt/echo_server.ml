module M = Moonpool
module M_lwt = Moonpool_lwt
module Trace = Trace_core

let () = Watchdog.arm ()
let ( let@ ) = ( @@ )
let await_lwt = M_lwt.await_lwt
let spf = Printf.sprintf

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

(* written via rename so readers never see a partial file *)
let write_port_file file port =
  let oc = open_out (file ^ ".tmp") in
  Printf.fprintf oc "%d\n" port;
  close_out oc;
  Sys.rename (file ^ ".tmp") file

let main ~port ~port_file ~verbose ~runner:_ () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let lwt_fut, _lwt_prom = Lwt.wait () in

  (* TODO: handle exit?? ctrl-c? *)
  let handle_client client_addr (ic, oc) : _ Lwt.t =
    let@ () = M_lwt.spawn_lwt in
    let _sp =
      Trace.enter_span ~parent:None ~__FILE__ ~__LINE__ "handle.client"
        ~data:(fun () -> [ "addr", `String (str_of_sockaddr client_addr) ])
    in

    if verbose then
      Printf.printf "got new client on %s\n%!" (str_of_sockaddr client_addr);

    let buf = Bytes.create 32 in
    let continue = ref true in
    while !continue do
      Trace.message "read";
      let n = Lwt_io.read_into ic buf 0 (Bytes.length buf) |> await_lwt in
      if n = 0 then
        continue := false
      else (
        Trace.messagef (fun k -> k "got %dB" n);
        Lwt_io.write_from_exactly oc buf 0 n |> await_lwt;
        Lwt_io.flush oc |> await_lwt;
        Trace.message "write"
      )
    done;
    if verbose then
      Printf.printf "done with client on %s\n%!" (str_of_sockaddr client_addr);
    Trace.exit_span _sp;
    Trace.message "exit handle client"
  in

  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let _server =
    Lwt_io.establish_server_with_client_address ~fd addr handle_client
    |> await_lwt
  in
  Printf.printf "listening\n%!";
  (match Lwt_unix.getsockname fd, port_file with
  | Unix.ADDR_INET (_, port), Some file -> write_port_file file port
  | _ -> ());

  M_lwt.await_lwt lwt_fut

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let port = ref 0 in
  let j = ref 4 in
  let verbose = ref false in
  let port_file = ref None in

  let opts =
    [
      "-v", Arg.Set verbose, " verbose";
      "-p", Arg.Set_int port, " port (0 for any free port)";
      "-j", Arg.Set_int j, " number of threads";
      ( "-port-file",
        Arg.String (fun f -> port_file := Some f),
        " write the actual port to this file once listening" );
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo server";

  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:!j () in
  (* Lwt_engine.set @@ new Lwt_engine.libev (); *)
  M_lwt.lwt_main @@ fun _ ->
  main ~runner ~port:!port ~port_file:!port_file ~verbose:!verbose ()
