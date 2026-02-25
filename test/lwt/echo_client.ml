module M_lwt = Moonpool_lwt
module Trace = Trace_core

let ci_mode = Option.is_some @@ Sys.getenv_opt "CI_MODE"
let spf = Printf.sprintf
let await_lwt = Moonpool_lwt.await_lwt
let ( let@ ) = ( @@ )

let main ~port ~n ~n_conn ~verbose ~msg_per_conn () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let t0 = Unix.gettimeofday () in
  Printf.printf
    "connecting to port %d (%d msg per conn, %d conns total, %d max at a time)\n\
     %!"
    port msg_per_conn n n_conn;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  let token_pool = Lwt_pool.create n_conn (fun () -> Lwt.return_unit) in
  let n_msg_total = ref 0 in

  let run_task () =
    (* Printf.printf "running task\n%!"; *)
    let@ () = Lwt_pool.use token_pool in

    let@ () = M_lwt.spawn_lwt in
    let _sp =
      Trace.enter_span ~parent:None ~__FILE__ ~__LINE__ "connect.client"
    in
    Trace.message "connecting new client…";

    let ic, oc = Lwt_io.open_connection addr |> await_lwt in

    let cleanup () =
      Trace.message "closing connection";
      Lwt_io.close ic |> await_lwt;
      Lwt_io.close oc |> await_lwt
    in

    let@ () = Fun.protect ~finally:cleanup in

    let buf = Bytes.create 32 in

    for _j = 1 to msg_per_conn do
      let _sp =
        Trace.enter_span ~parent:(Some _sp) ~__FILE__ ~__LINE__ "write.loop"
      in

      let s = spf "hello %d" _j in
      Lwt_io.write oc s |> await_lwt;
      Lwt_io.flush oc |> await_lwt;
      incr n_msg_total;

      (* read back something *)
      Lwt_io.read_into_exactly ic buf 0 (String.length s) |> await_lwt;
      if verbose then
        Printf.printf "read: %s\n%!" (Bytes.sub_string buf 0 (String.length s));
      Trace.exit_span _sp;
      ()
    done;
    Trace.exit_span _sp
  in

  (* start the first [n_conn] tasks *)
  let futs = List.init n (fun _ -> run_task ()) in
  Lwt.join futs |> await_lwt;

  Printf.printf "all done\n%!";
  let elapsed = Unix.gettimeofday () -. t0 in
  if not ci_mode then
    Printf.printf "  sent %d messages in %.4fs (%.2f msg/s)\n%!" !n_msg_total
      elapsed
      (float !n_msg_total /. elapsed);
  ()

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";

  let port = ref 0 in
  let n_conn = ref 100 in
  let n = ref 50_000 in
  let msg_per_conn = ref 10 in
  let verbose = ref false in

  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "-n", Arg.Set_int n, " total number of connections";
      ( "--msg-per-conn",
        Arg.Set_int msg_per_conn,
        " messages sent per connection" );
      "-v", Arg.Set verbose, " verbose";
      ( "--n-conn",
        Arg.Set_int n_conn,
        " maximum number of connections opened simultaneously" );
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo client";

  let main () =
    (* Lwt_engine.set @@ new Lwt_engine.libev (); *)
    M_lwt.lwt_main @@ fun _runner ->
    main ~port:!port ~n:!n ~n_conn:!n_conn ~verbose:!verbose
      ~msg_per_conn:!msg_per_conn ()
  in

  print_endline "first run";
  main ();
  assert (not (M_lwt.is_setup ()));
  print_endline "second run";
  main ();
  assert (not (M_lwt.is_setup ()));
  print_endline "done"
