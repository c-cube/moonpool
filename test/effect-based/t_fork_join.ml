[@@@ifge 5.0]

open! Moonpool

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

let () =
  let total_sum = Atomic.make 0 in

  Pool.run_wait_block pool (fun () ->
      Fork_join.for_ ~chunk_size:5 100 (fun low high ->
          (* iterate on the range sequentially. The range should have 5 items or less. *)
          let local_sum = ref 0 in
          for i = low to high do
            local_sum := !local_sum + i
          done;
          ignore (Atomic.fetch_and_add total_sum !local_sum : int)));
  assert (Atomic.get total_sum = 4950)

[@@@endif]
