module M_lwt = Moonpool_lwt

let ( let@ ) = ( @@ )

let () =
  let@ runner = M_lwt.lwt_main in
  T_fibers.Fib.run1 ~runner ();
  T_fibers.Fib.run2 ~runner ()
