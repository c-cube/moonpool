open! Moonpool

let ( let@ ) = ( @@ )

(* test proper resource handling *)
let () =
  let a = Atomic.make 0 in
  for _i = 1 to 1_000 do
    (* allocate a new pool at each iteration *)
    let@ p = Pool.with_ ~min:4 () in
    Pool.run_wait_block p (fun () -> Atomic.incr a)
  done;
  assert (Atomic.get a = 1_000)
