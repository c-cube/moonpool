open Moonpool
module M_lwt = Moonpool_lwt
module F = Moonpool_fib

let ( let@ ) = ( @@ )

let () =
  (* run fibers in the background, await them in the main thread *)
  let@ bg = Fifo_pool.with_ ~num_threads:4 () in
  let r =
    M_lwt.lwt_main @@ fun runner ->
    let f1 = F.spawn_top ~on:bg (fun () -> 1) in
    let f2 = F.spawn_top ~on:runner (fun () -> 2) in
    let f3 = F.spawn_top ~on:runner (fun () -> F.await f1 + 10) in
    let r = F.await f2 + F.await f3 in
    assert (r = 13);
    r
  in
  assert (r = 13)

let () =
  Printf.eprintf "PART 2\n%!";
  try
    let _r =
      M_lwt.lwt_main @@ fun runner ->
      let fib = F.spawn_top ~on:runner (fun () -> failwith "oops") in
      F.await fib
    in

    assert false
  with Failure msg ->
    (* Printf.eprintf "got %S\n%!" msg; *)
    assert (msg = "oops")
