module M = Moonpool
module MU = Moonpool_io
module Trace = Trace_core

let ( let@ ) = ( @@ )

let main ~port ~j () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in
  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:j () in

  let@ _main_runner = MU.main in
  Trace.set_thread_name "main";

  let@ server =
    MU.TCP_server.with_server ~runner (MU.Sockaddr.any port)
      ~handle:(fun ~client_addr:addr ic oc ->
        Trace.message "got new client";
        let@ _sp =
          Trace.with_span ~__FILE__ ~__LINE__ "handle.client" ~data:(fun () ->
              [ "addr", `String (MU.Sockaddr.show addr) ])
        in

        let buf = Bytes.create 32 in
        let continue = ref true in
        while !continue do
          Trace.message "read";
          let n = Iostream.In_buf.input ic buf 0 (Bytes.length buf) in
          if n = 0 then continue := false;
          Trace.messagef (fun k -> k "got %dB" n);
          Iostream.Out_buf.output oc buf 0 n;
          Iostream.Out_buf.flush oc;
          Trace.message "write"
          (* MU.sleep_s 0.02 *)
        done)
  in

  Trace.messagef (fun k ->
      k "server established on %s"
        (MU.Sockaddr.show @@ MU.TCP_server.endpoint server));
  Printf.printf "listening on %s\n%!"
    (MU.Sockaddr.show @@ MU.TCP_server.endpoint server);

  Trace.message "awaiting server";
  MU.TCP_server.await server;
  ()

let () =
  Sys.catch_break true;
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "entry";
  let port = ref 0 in
  let j = ref 4 in

  let opts =
    [
      "-p", Arg.Set_int port, " port"; "j", Arg.Set_int j, " number of threads";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo server";

  main ~port:!port ~j:!j ()
