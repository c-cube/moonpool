open Moonpool

let pool = Pool.create ~min:4 ()

let rec fib x =
  if x <= 1 then
    1
  else
    fib (x - 1) + fib (x - 2)

let () = assert (List.init 10 fib = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])
let fibs = Array.init 30 (fun n -> Fut.spawn ~on:pool (fun () -> fib n))
let res = Fut.join_array fibs |> Fut.wait_block

let () =
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
