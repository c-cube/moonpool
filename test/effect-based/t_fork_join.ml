open Moonpool

let pool = Pool.create ~min:4 ()

let () =
  let x =
    Pool.run_wait_block pool (fun () ->
        let x, y =
          Fork_join.both
            (fun () ->
              Thread.delay 0.005;
              1)
            (fun () ->
              Thread.delay 0.005;
              2)
        in
        x + y)
  in
  assert (x = 3)

let () =
  try
    Pool.run_wait_block pool (fun () ->
        Fork_join.both_ignore
          (fun () -> Thread.delay 0.005)
          (fun () ->
            Thread.delay 0.02;
            raise Exit));
    failwith "should fail"
  with Exit -> ()

let () =
  let par_sum =
    Pool.run_wait_block pool (fun () ->
        Fork_join.all_init 42 (fun i -> i * i) |> List.fold_left ( + ) 0)
  in
  let exp_sum = List.init 42 (fun x -> x * x) |> List.fold_left ( + ) 0 in
  assert (par_sum = exp_sum)
