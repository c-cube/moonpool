module M = Moonpool
module M_lwt = Moonpool_lwt
module Trace = Trace_core

let ( let@ ) = ( @@ )

let main ~port ~runner ~n ~n_conn () : unit Lwt.t =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in

  let remaining = Atomic.make n in
  let all_done = Atomic.make 0 in

  let fut_exit, prom_exit = M.Fut.make () in

  Printf.printf "connecting to port %d\n%!" port;
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in

  let rec run_task () =
    (* Printf.printf "running task\n%!"; *)
    let n = Atomic.fetch_and_add remaining (-1) in
    if n > 0 then (
      ( (* let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "connect.client" in *)
        M_lwt.TCP_client.with_connect addr
      @@ fun ic oc ->
        let buf = Bytes.create 32 in

        for _j = 1 to 100 do
          let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "write.loop" in
          M_lwt.IO_out.output_string oc "hello";

          (* read back something *)
          let _n = M_lwt.IO_in.really_input ic buf in
          ()
        done );

      (* run another task *) M.Runner.run_async runner run_task
    ) else (
      (* if we're the last to exit, resolve the promise *)
      let n_already_done = Atomic.fetch_and_add all_done 1 in
      if n_already_done = n_conn - 1 then (
        Printf.printf "all done\n%!";
        M.Fut.fulfill prom_exit @@ Ok ()
      )
    )
  in

  (* start the first [n_conn] tasks *)
  for _i = 1 to n_conn do
    M.Runner.run_async runner run_task
  done;

  (* exit when [fut_exit] is resolved *)
  M_lwt.lwt_of_fut fut_exit

let () =
  let@ () = Trace_tef.with_setup () in
  Trace.set_thread_name "main";
  let port = ref 0 in
  let j = ref 4 in
  let n_conn = ref 100 in
  let n = ref 50_000 in

  let opts =
    [
      "-p", Arg.Set_int port, " port";
      "j", Arg.Set_int j, " number of threads";
      "-n", Arg.Set_int n, " total number of connections";
      "--n-conn", Arg.Set_int n_conn, " number of parallel connections";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "echo client";

  let@ runner = M.Ws_pool.with_ ~name:"tpool" ~num_threads:!j () in
  Lwt_engine.set @@ new Lwt_engine.libev ();
  Lwt_main.run @@ main ~runner ~port:!port ~n:!n ~n_conn:!n_conn ()
