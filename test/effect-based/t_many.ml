[@@@ifge 5.0]

open Moonpool

let pool = Pool.create ~min:4 ()

let run () =
  let t1 = Unix.gettimeofday () in

  let n = 1_000_000 in
  let n_tasks = 3 in
  let task () =
    let l = List.init n (fun x -> Fut.spawn ~on:pool (fun () -> x)) in
    Fut.spawn ~on:pool (fun () ->
        List.fold_left
          (fun n x ->
            let _res = Fut.await x in
            n + 1)
          0 l)
  in

  let futs =
    List.init n_tasks (fun _ -> Fut.spawn ~on:pool task |> Fut.join ~on:pool)
  in

  let lens = List.map Fut.wait_block_exn futs in
  Printf.printf "awaited %d items (%d times)\n%!" (List.hd lens) n_tasks;
  Printf.printf "in %.4fs\n%!" (Unix.gettimeofday () -. t1);
  assert (List.for_all (fun s -> s = n) lens)

let () = run ()

[@@@endif]
