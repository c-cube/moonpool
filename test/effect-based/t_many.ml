open Moonpool

let ( let@ ) = ( @@ )

let run ~pool () =
  let t1 = Unix.gettimeofday () in

  let n = 200_000 in
  let n_tasks = 3 in
  let task () =
    let l = List.init n (fun x -> Fut.spawn ~on:pool (fun () -> x)) in
    Fut.spawn ~on:pool (fun () ->
        List.fold_left
          (fun n x ->
            let _res = Sys.opaque_identity (Fut.await x) in
            n + 1)
          0 l)
  in

  let futs = List.init n_tasks (fun _ -> Fut.spawn ~on:pool task |> Fut.join) in

  let lens = List.map Fut.wait_block_exn futs in
  Printf.printf "awaited %d items (%d times)\n%!" (List.hd lens) n_tasks;
  Printf.printf "in %.4fs\n%!" (Unix.gettimeofday () -. t1);
  assert (List.for_all (fun s -> s = n) lens)

let () =
  (print_endline "with fifo";
   let@ pool = Fifo_pool.with_ ~num_threads:4 () in
   run ~pool ());

  (print_endline "with WS(1)";
   let@ pool = Ws_pool.with_ ~num_threads:1 () in
   run ~pool ());

  (print_endline "with WS(2)";
   let@ pool = Ws_pool.with_ ~num_threads:2 () in
   run ~pool ());

  (print_endline "with WS(4)";
   let@ pool = Ws_pool.with_ ~num_threads:4 () in
   run ~pool ());

  ()
