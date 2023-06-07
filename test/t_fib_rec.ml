open Moonpool

let rec fib_direct x =
  if x <= 1 then
    1
  else
    fib_direct (x - 1) + fib_direct (x - 2)

let rec fib ~on x : int Fut.t =
  if x <= 18 then
    Fut.spawn ~on (fun () -> fib_direct x)
  else
    let open Fut.Infix_local in
    let+ t1 = fib ~on (x - 1) and+ t2 = fib ~on (x - 2) in
    t1 + t2

let () = assert (List.init 10 fib_direct = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])

let fib_40 : int =
  let pool = Pool.create ~min:8 () in
  fib ~on:pool 40 |> Fut.wait_block_exn

let run_test () =
  let pool = Pool.create ~min:8 () in

  assert (
    List.init 10 (fib ~on:pool)
    |> Fut.join_list |> Fut.wait_block_exn
    = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ]);

  let fibs = Array.init 5 (fun _ -> fib ~on:pool 40) in

  let res = Fut.join_array fibs |> Fut.wait_block in
  Pool.shutdown pool;

  assert (res = Ok (Array.make 5 fib_40))

let () =
  Printf.printf "fib 40 = %d\n%!" fib_40;
  for _i = 1 to 2 do
    run_test ()
  done;

  (* now make sure we can do this with multiple pools in parallel *)
  let jobs = Array.init 4 (fun _ -> Thread.create run_test ()) in
  Array.iter Thread.join jobs
