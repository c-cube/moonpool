module M = Moonpool
module MU = Moonpool_unix
module Trace = Trace_core

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let str_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (addr, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr addr) port

let main ~port ~j () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in
  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:j () in

  let@ () = MU.main ~runner in
  Trace.set_thread_name "main";
  Printf.printf "IN MAIN\n%!";

  MU.TCP_server.with_server ~port ~runner ()
    ~after_init:(fun self ->
      Printf.printf "listening on port %d\n%!" (MU.TCP_server.port self))
    ~handle_client:(fun _server addr ic oc ->
      let@ _sp =
        Trace.with_span ~__FILE__ ~__LINE__ "handle.client" ~data:(fun () ->
            [ "addr", `String (str_of_sockaddr addr) ])
      in

      let buf = Bytes.create 32 in
      let continue = ref true in
      while !continue do
        Trace.message "read";
        let n = MU.IO_in.input ic buf 0 (Bytes.length buf) in
        if n = 0 then continue := false;
        Trace.messagef (fun k -> k "got %dB" n);
        MU.IO_out.output oc buf 0 n;
        MU.IO_out.flush oc;
        Trace.message "write"
        (* MU.sleep_s 0.02 *)
      done)

let () =
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
