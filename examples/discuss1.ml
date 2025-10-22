(** NOTE: this was an example from
    https://discuss.ocaml.org/t/confused-about-moonpool-cancellation/15381 but
    there is no cancelation anymore :) *)

let ( let@ ) = ( @@ )

let () =
  let@ () = Trace_tef.with_setup () in
  let@ _ = Moonpool.main in

  (* let@ runner = Moonpool.Ws_pool.with_ () in *)
  let@ runner = Moonpool.Background_thread.with_ () in

  (* Pretend this is some long-running read loop *)
  for i = 1 to 10 do
    Printf.printf "MAIN LOOP %d\n%!" i;
    let _ : _ Moonpool.Fut.t =
      Moonpool.Fut.spawn ~on:runner (fun () ->
          Printf.printf "RUN FIBER %d\n%!" i;
          Format.printf "FIBER %d NOT CANCELLED YET@." i;
          failwith "BOOM")
    in
    Moonpool.Fut.yield ();
    (* Thread.delay 0.2; *)
    (* Thread.delay 0.0001; *)
    ()
  done
