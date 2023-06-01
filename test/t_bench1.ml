open Moonpool

let ( let@ ) = ( @@ )

let rec fib x =
  if x <= 1 then
    1
  else
    fib (x - 1) + fib (x - 2)

let run ~psize ~n ~j () : _ Fut.t =
  Printf.printf "pool size=%d, n=%d, j=%d\n%!" psize n j;
  let pool =
    Pool.create
      ~on_init_thread:(fun ~dom_id:_ ~t_id () ->
        Tracy.name_thread (Printf.sprintf "t_%d" t_id))
      ~min:psize ~per_domain:0 ()
  in

  let loop () =
    let@ _sp = Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"loop" () in
    for _i = 1 to n do
      let () =
        let@ _sp = Tracy.with_ ~file:__FILE__ ~line:__LINE__ ~name:"iter" () in
        Tracy.add_text _sp (string_of_int _i);
        ignore (Sys.opaque_identity (fib 30) : int)
      in

      Thread.yield ()
    done
  in

  let fut = Fut.for_ ~on:pool j (fun _ -> loop ()) in
  fut

let () =
  Tracy.enable ();
  let j = ref 10 in
  let n = ref 10 in
  let psize = ref 4 in
  let opts =
    [
      "-psize", Arg.Set_int psize, " pool size";
      "-j", Arg.Set_int j, " number of tasks";
      "-n", Arg.Set_int n, " number of iterations per task";
    ]
    |> Arg.align
  in
  Arg.parse opts ignore "";

  (* start two distinct pools *)
  let fut1 = run ~psize:!psize ~n:!n ~j:!j () in
  let fut2 = run ~psize:!psize ~n:!n ~j:!j () in

  Fut.wait_block_exn fut1;
  Fut.wait_block_exn fut2;
  Printf.printf "done\n%!"
