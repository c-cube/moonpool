open! Moonpool

let ( let@ ) = ( @@ )

let () =
  let sum = Atomic.make 0 in
  let n = 100_000 in

  (let@ pool = Ws_pool.with_ ~num_threads:30 () in
   for _i = 1 to n do
     Runner.run_async pool (fun () ->
         Thread.delay 0.0001;
         ignore (Atomic.fetch_and_add sum 20 : int))
   done;
   Runner.shutdown pool);

  (* make sure that shutdown didn't terminate before
     all tasks were run *)
  let sum = Atomic.get sum in
  let expected = 20 * n in

  Printf.printf "sum=%d, expected=%d\n%!" sum expected;
  assert (sum = expected);
  ()
