open Moonpool

let rec fib_direct x =
  if x <= 1 then
    1
  else
    fib_direct (x - 1) + fib_direct (x - 2)

let fib ~on x : int Fut.t =
  let rec fib_rec x : int =
    if x <= 18 then
      fib_direct x
    else (
      let t1 = Fut.spawn ~on (fun () -> fib_rec (x - 1))
      and t2 = Fut.spawn ~on (fun () -> fib_rec (x - 2)) in
      Fut.await_exn t1 + Fut.await_exn t2
    )
  in
  Fut.spawn ~on (fun () -> fib_rec x)

let () = Tracy_client_trace.setup ()
let () = assert (List.init 10 fib_direct = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])

let fib_40 : int =
  let pool = Pool.create ~min:8 () in
  fib ~on:pool 40 |> Fut.wait_block_exn

let () = Printf.printf "fib 40 = %d\n%!" fib_40

let run_test () =
  let pool = Pool.create ~min:8 () in

  assert (
    List.init 10 (fib ~on:pool)
    |> Fut.join_list |> Fut.wait_block_exn
    = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ]);

  let fibs = Array.init 3 (fun _ -> fib ~on:pool 40) in

  let res = Fut.join_array fibs |> Fut.wait_block in
  Pool.shutdown pool;

  assert (res = Ok (Array.make 3 fib_40))

let () =
  (* now make sure we can do this with multiple pools in parallel *)
  let jobs = Array.init 2 (fun _ -> Thread.create run_test ()) in
  Array.iter Thread.join jobs
