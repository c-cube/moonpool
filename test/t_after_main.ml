open! Moonpool

let () = Watchdog.arm ()
let () = Main.main (fun _runner -> ())

let () =
  (* after Main.main returns, there is no current runner anymore and
     auto-detecting combinators fall back to the calling thread *)
  assert (Runner.get_current_runner () = None);
  assert (Fut.wait_block_exn (Fut.map ~f:(fun x -> x + 1) (Fut.return 41)) = 42)
