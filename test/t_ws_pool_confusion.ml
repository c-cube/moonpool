open Moonpool

let delay () = Thread.delay 0.001

let run ~p_main:_ ~p_sub () =
  let f1 =
    Fut.spawn ~on:p_sub (fun () ->
        delay ();
        1)
  in
  let f2 =
    Fut.spawn ~on:p_sub (fun () ->
        delay ();
        2)
  in
  Fut.wait_block_exn f1 + Fut.wait_block_exn f2

let () =
  let p_main = Ws_pool.create ~num_threads:2 () in
  let p_sub = Ws_pool.create ~num_threads:10 () in

  let futs = List.init 8 (fun _ -> Fut.spawn ~on:p_main (run ~p_main ~p_sub)) in

  let l = List.map Fut.wait_block_exn futs in
  assert (l = List.init 8 (fun _ -> 3));

  print_endline "ok";
  ()
