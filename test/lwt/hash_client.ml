module M = Moonpool
module M_lwt = Moonpool_lwt
module Trace = Trace_core

module Str_tbl = Hashtbl.Make (struct
  include String

  let hash = Hashtbl.hash
end)

let await_lwt = Moonpool_lwt.await_lwt
let ( let@ ) = ( @@ )

let main ~port ~ext ~dir ~n_conn () : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  Printf.printf "hash dir=%S\n%!" dir;

  Printf.printf "connecting to port %d\n%!" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  (* TODO: *)
  let run_task () : unit Lwt.t =
    let@ () = M_lwt.spawn_lwt in
    let _sp =
      Trace.enter_manual_span ~parent:None ~__FILE__ ~__LINE__ "run-task"
    in

    let seen = Str_tbl.create 16 in

    let ic, oc = Lwt_io.open_connection addr |> await_lwt in
    let rec walk file : unit =
      if not (Sys.file_exists file) then
        ()
      else if Str_tbl.mem seen file then
        ()
      else if Sys.is_directory file then (
        let _sp =
          Trace.enter_manual_span
            ~parent:(Some (Trace.ctx_of_span _sp))
            ~__FILE__ ~__LINE__ "walk-dir"
            ~data:(fun () -> [ "d", `String file ])
        in

        Str_tbl.add seen file ();
        let d = Sys.readdir file in
        Array.sort String.compare d;
        Array.iter (fun sub -> walk (Filename.concat file sub)) d
      ) else if ext <> "" && Filename.extension file <> ext then
        ()
      else (
        Str_tbl.add seen file ();
        Lwt_io.write_line oc file |> await_lwt;
        let res = Lwt_io.read_line ic |> await_lwt in
        Printf.printf "%s\n%!" res
      )
    in
    walk dir;
    Trace.exit_manual_span _sp
  in

  (* start the first [n_conn] tasks *)
  let futs = List.init n_conn (fun _ -> run_task ()) in
  Lwt.join futs |> await_lwt

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";

  let port = ref 1234 in
  let n_conn = ref 100 in
  let ext = ref "" in
  let dir = ref "." in

  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "-d", Arg.Set_string dir, " directory to hash";
      "--n-conn", Arg.Set_int n_conn, " number of parallel connections";
      "--ext", Arg.Set_string ext, " extension to filter files";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo client";

  (* Lwt_engine.set @@ new Lwt_engine.libev (); *)
  M_lwt.lwt_main @@ fun _runner ->
  main ~port:!port ~ext:!ext ~dir:!dir ~n_conn:!n_conn ()
