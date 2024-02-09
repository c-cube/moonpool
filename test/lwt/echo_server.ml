module M = Moonpool
module M_lwt = Moonpool_lwt
module Trace = Trace_core

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let main ~port ~runner () : unit Lwt.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let lwt_fut, _lwt_prom = Lwt.wait () in

  (* TODO: handle exit?? *)
  Printf.printf "listening on port %d\n%!" port;

  let handle_client client_addr ic oc =
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__ "handle.client" ~data:(fun () ->
          [ "addr", `String (str_of_sockaddr client_addr) ])
    in

    let buf = Bytes.create 32 in
    let continue = ref true in
    while !continue do
      let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "read.loop" in
      Trace.message "read";
      let n = M_lwt.IO_in.input ic buf 0 (Bytes.length buf) in
      if n = 0 then
        continue := false
      else (
        Trace.messagef (fun k -> k "got %dB" n);
        M_lwt.IO_out.output oc buf 0 n;
        M_lwt.IO_out.flush oc;
        Trace.message "write" (* MU.sleep_s 0.02 *)
      )
    done;
    Trace.message "exit handle client"
  in

  let addr = Unix.ADDR_INET (Unix.inet_addr_any, port) in
  let _server = M_lwt.TCP_server.establish ~runner addr handle_client in

  lwt_fut

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let port = ref 0 in
  let j = ref 4 in

  let opts =
    [
      "-p", Arg.Set_int port, " port"; "j", Arg.Set_int j, " number of threads";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo server";

  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:!j () in
  Lwt_engine.set @@ new Lwt_engine.libev ();
  Lwt_main.run @@ main ~runner ~port:!port ()
