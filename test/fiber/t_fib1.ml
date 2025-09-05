let ( let@ ) = ( @@ )

let () =
  let@ runner = Moonpool_fib.main in
  T_fibers.Fib.run1 ~runner ();
  T_fibers.Fib.run2 ~runner ()
