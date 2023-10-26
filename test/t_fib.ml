open Moonpool

let ( let@ ) = ( @@ )

let with_pool ~kind () f =
  match kind with
  | `Fifo_pool -> Fifo_pool.with_ ~min:4 () f
  | `Pool -> Pool.with_ ~min:4 () f

let rec fib x =
  if x <= 1 then
    1
  else
    fib (x - 1) + fib (x - 2)

let () = assert (List.init 10 fib = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])

let run_test ~pool () =
  let fibs = Array.init 30 (fun n -> Fut.spawn ~on:pool (fun () -> fib n)) in
  let res = Fut.join_array fibs |> Fut.wait_block in
  Pool.shutdown pool;

  assert (
    res
    = Ok
        [|
          1;
          1;
          2;
          3;
          5;
          8;
          13;
          21;
          34;
          55;
          89;
          144;
          233;
          377;
          610;
          987;
          1597;
          2584;
          4181;
          6765;
          10946;
          17711;
          28657;
          46368;
          75025;
          121393;
          196418;
          317811;
          514229;
          832040;
        |])

let run ~kind () =
  for _i = 1 to 4 do
    let@ pool = with_pool ~kind () in
    run_test ~pool ()
  done;

  (* now make sure we can do this with multiple pools in parallel *)
  let jobs =
    Array.init 4 (fun _ ->
        Thread.create
          (fun () ->
            let@ pool = with_pool ~kind () in
            run_test ~pool ())
          ())
  in
  Array.iter Thread.join jobs

let () =
  run ~kind:`Pool ();
  run ~kind:`Fifo_pool ()
