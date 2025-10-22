let ( let@ ) = ( @@ )

let () =
  let@ runner = Moonpool.main in
  T_fibers.Fib.run1 ~runner ();
  T_fibers.Fib.run2 ~runner ()
