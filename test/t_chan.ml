open! Moonpool

let () = Watchdog.arm ()
let pool = Ws_pool.create ~num_threads:2 ()

let () =
  (* a channel with max_size 0 has bound 0, not 1 *)
  let c = Chan.create ~max_size:0 () in
  assert (not (Chan.try_push c 1));
  let fut = Fut.spawn ~on:pool (fun () -> Chan.push c 1) in
  Chan.close c;
  assert (Result.is_error (Fut.wait_block fut))

let () =
  (* max_size 1 accepts exactly one element *)
  let c = Chan.create ~max_size:1 () in
  assert (Chan.try_push c 1);
  assert (not (Chan.try_push c 2));
  assert (Chan.pop c = 1);
  assert (Chan.try_push c 2)

let () =
  (* push wakes up a popper suspended on an empty queue *)
  let c = Chan.create ~max_size:2 () in
  let pop_fut = Fut.spawn ~on:pool (fun () -> Chan.pop c) in
  Thread.delay 0.5;
  assert (Chan.try_push c 42);
  assert (Fut.wait_block_exn pop_fut = 42)

let () = Runner.shutdown pool
