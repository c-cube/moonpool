module M = Moonpool
module M_lwt = Moonpool_lwt
module Trace = Trace_core

let ( let@ ) = ( @@ )

let main ~port ~runner ~dir ~n_conn () : unit Lwt.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  Printf.printf "hash dir=%S\n%!" dir;

  Printf.printf "connecting to port %d\n%!" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  (* TODO: *)
  let run_task () : unit =
    let _sp = Trace.enter_manual_toplevel_span ~__FILE__ ~__LINE__ "run-task" in
    M_lwt.TCP_client.with_connect' addr @@ fun ic oc ->
    let rec walk file : unit =
      if not (Sys.file_exists file) then
        ()
      else if Sys.is_regular_file file then (
        M_lwt.run_in_lwt_and_await (fun () -> Lwt_io.write_line oc file);
        let res = M_lwt.run_in_lwt_and_await (fun () -> Lwt_io.read_line ic) in
        Printf.printf "%s\n%!" res
      ) else if Sys.is_directory file then (
        let _sp =
          Trace.enter_manual_sub_span ~parent:_sp ~__FILE__ ~__LINE__ "walk-dir"
            ~data:(fun () -> [ "d", `String file ])
        in

        Printf.printf "explore %S\n%!" file;
        let d = Sys.readdir file in
        Array.sort String.compare d;
        Array.iter (fun sub -> walk (Filename.concat file sub)) d
      )
    in
    walk dir;
    Trace.exit_manual_span _sp
  in

  (* start the first [n_conn] tasks *)
  let futs = List.init n_conn (fun _ -> M.Fut.spawn ~on:runner run_task) in

  Lwt.join (List.map M_lwt.lwt_of_fut futs)

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";

  let port = ref 1234 in
  let j = ref 4 in
  let n_conn = ref 100 in
  let dir = ref "." in

  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "-j", Arg.Set_int j, " number of threads";
      "-d", Arg.Set_string dir, " directory to hash";
      "--n-conn", Arg.Set_int n_conn, " number of parallel connections";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo client";

  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:!j () in
  Lwt_engine.set @@ new Lwt_engine.libev ();
  Lwt_main.run @@ main ~runner ~port:!port ~dir:!dir ~n_conn:!n_conn ()
