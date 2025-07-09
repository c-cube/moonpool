let ( let@ ) = ( @@ )

open Moonpool
module FJ = Moonpool_forkjoin

let rec fib_direct x =
  if x <= 1 then
    1
  else
    fib_direct (x - 1) + fib_direct (x - 2)

let rec fib x : int =
  (* some cutoff for sequential computation *)
  if x <= 18 then
    fib_direct x
  else (
    let n1, n2 = FJ.both (fun () -> fib (x - 1)) (fun () -> fib (x - 2)) in
    n1 + n2
  )

let fib_40 : int =
  let@ pool = Ws_pool.with_ ~num_threads:8 () in
  Fut.spawn ~on:pool (fun () -> fib 40) |> Fut.wait_block_exn

let () = Printf.printf "fib 40 = %d\n%!" fib_40

let run_test () =
  let@ pool = Ws_pool.with_ ~num_threads:8 () in

  let fut =
    Fut.spawn ~on:pool (fun () ->
        let fibs = FJ.all_init 3 (fun _ -> fib 40) in
        fibs)
  in

  let res = Fut.wait_block_exn fut in
  Ws_pool.shutdown pool;

  assert (res = (Array.make 3 fib_40 |> Array.to_list))

let () =
  (* now make sure we can do this with multiple pools in parallel *)
  let jobs = Array.init 2 (fun _ -> Thread.create run_test ()) in
  Array.iter Thread.join jobs
