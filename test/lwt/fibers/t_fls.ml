module M_lwt = Moonpool_lwt

let ( let@ ) = ( @@ )

let () =
  (let@ runner = M_lwt.lwt_main in
   T_fibers.Fls.run ~pool:runner ~pool_name:"lwt" ());
  ()
