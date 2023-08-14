open! Moonpool

let ( let@ ) = ( @@ )

(* test proper resource handling *)
let () =
  let a = Atomic.make 0 in
  for _i = 1 to 1_000 do
    (* give a chance to domains to die *)
    if _i mod 100 = 0 then Thread.delay 0.8;

    (* allocate a new pool at each iteration *)
    let@ p = Pool.with_ ~min:4 () in
    Pool.run_wait_block p (fun () -> Atomic.incr a)
  done;
  assert (Atomic.get a = 1_000)
