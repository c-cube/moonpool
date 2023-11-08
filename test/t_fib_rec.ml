open! Moonpool

let ( let@ ) = ( @@ )

let rec fib_direct x =
  if x <= 1 then
    1
  else
    fib_direct (x - 1) + fib_direct (x - 2)

let n_calls_fib_direct = Atomic.make 0

let rec fib ~on x : int Fut.t =
  if x <= 18 then
    Fut.spawn ~on (fun () ->
        Atomic.incr n_calls_fib_direct;
        fib_direct x)
  else
    let open Fut.Infix in
    let+ t1 = fib ~on (x - 1) and+ t2 = fib ~on (x - 2) in
    t1 + t2

let () = assert (List.init 10 fib_direct = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ])

let fib_40 : int lazy_t =
  lazy
    (let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "fib40" in
     let pool = Fifo_pool.create ~num_threads:8 () in
     let r = fib ~on:pool 40 |> Fut.wait_block_exn in
     Ws_pool.shutdown pool;
     r)

let run_test ~pool () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run-test" in

  let (lazy fib_40) = fib_40 in

  assert (
    List.init 10 (fib ~on:pool)
    |> Fut.join_list |> Fut.wait_block_exn
    = [ 1; 1; 2; 3; 5; 8; 13; 21; 34; 55 ]);

  let n_fibs = 3 in
  let fibs = Array.init n_fibs (fun _ -> fib ~on:pool 40) in

  let res = Fut.join_array fibs |> Fut.wait_block in

  assert (res = Ok (Array.make n_fibs fib_40))

let run_test_size ~size () =
  Printf.printf "test pool(%d)\n%!" size;
  let@ pool = Ws_pool.with_ ~num_threads:size () in
  run_test ~pool ()

let run_test_fifo ~size () =
  Printf.printf "test fifo(%d)\n%!" size;
  let@ pool = Fifo_pool.with_ ~num_threads:size () in
  run_test ~pool ()

let setup_counter () =
  if Trace.enabled () then
    ignore
      (Thread.create
         (fun () ->
           while true do
             Thread.delay 0.01;
             Trace.counter_int "n-fib-direct" (Atomic.get n_calls_fib_direct)
           done)
         ()
        : Thread.t)

let () =
  let@ () = Trace_tef.with_setup () in
  setup_counter ();

  let (lazy fib_40) = fib_40 in
  Printf.printf "fib 40 = %d\n%!" fib_40;

  run_test_fifo ~size:4 ();

  List.iter (fun size -> run_test_size ~size ()) [ 1; 2; 4; 8 ];

  (* now make sure we can do this with multiple pools in parallel *)
  let jobs = Array.init 4 (fun _ -> Thread.create (run_test_size ~size:4) ()) in
  Array.iter Thread.join jobs
