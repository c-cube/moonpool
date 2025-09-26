open! Moonpool

let ( let@ ) = ( @@ )

let () =
  let@ _ = Moonpool_fib.main in
  (let@ pool = Ws_pool.with_ () in
   T_fibers.Fls.run ~pool ~pool_name:"ws_pool" ());

  (let@ pool = Fifo_pool.with_ () in
   T_fibers.Fls.run ~pool ~pool_name:"fifo_pool" ());
  ()
