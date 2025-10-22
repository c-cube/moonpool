open Moonpool
module F = Moonpool.Fut

let ( let@ ) = ( @@ )

let () =
  let r =
    Moonpool.main @@ fun runner ->
    let f1 = F.spawn ~on:runner (fun () -> 1) in
    let f2 = F.spawn ~on:runner (fun () -> 2) in
    let f3 = F.spawn ~on:runner (fun () -> F.await f1 + 10) in
    let r = F.await f2 + F.await f3 in
    assert (r = 13);
    r
  in
  assert (r = 13)

let () =
  (* run fibers in the background, await them in the main thread *)
  let@ bg = Fifo_pool.with_ ~num_threads:4 () in
  let r =
    Moonpool.main @@ fun runner ->
    let f1 = F.spawn ~on:bg (fun () -> 1) in
    let f2 = F.spawn ~on:runner (fun () -> 2) in
    let f3 = F.spawn ~on:bg (fun () -> F.await f1 + 10) in
    let r = F.await f2 + F.await f3 in
    assert (r = 13);
    r
  in
  assert (r = 13)

let () =
  try
    let _r =
      Moonpool.main @@ fun runner ->
      let fib = F.spawn ~on:runner (fun () -> failwith "oops") in
      F.await fib
    in

    assert false
  with Failure msg ->
    (* Printf.eprintf "got %S\n%!" msg; *)
    assert (msg = "oops")
