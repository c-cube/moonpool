(** Example from
    https://discuss.ocaml.org/t/confused-about-moonpool-cancellation/15381 *)

let ( let@ ) = ( @@ )

let () =
  let@ () = Trace_tef.with_setup () in
  let@ _ = Moonpool_fib.main in

  (* let@ runner = Moonpool.Ws_pool.with_ () in *)
  let@ runner = Moonpool.Background_thread.with_ () in

  (* Pretend this is some long-running read loop *)
  for i = 1 to 10 do
    Printf.printf "MAIN LOOP %d\n%!" i;
    Moonpool_fib.check_if_cancelled ();
    let _ : _ Moonpool_fib.t =
      Moonpool_fib.spawn ~on:runner ~protect:false (fun () ->
          Printf.printf "RUN FIBER %d\n%!" i;
          Moonpool_fib.check_if_cancelled ();
          Format.printf "FIBER %d NOT CANCELLED YET@." i;
          failwith "BOOM")
    in
    Moonpool_fib.yield ();
    (* Thread.delay 0.2; *)
    (* Thread.delay 0.0001; *)
    ()
  done
