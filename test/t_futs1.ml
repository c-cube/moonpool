open! Moonpool

let pool = Pool.create ~min:4 ()
let pool2 = Pool.create ~min:2 ()

let () =
  let fut = Fut.return 1 in
  assert (Fut.peek fut = Some (Ok 1));
  assert (Fut.is_resolved fut)

let () =
  let fut = Fut.fail Not_found (Printexc.get_callstack 1) in
  assert (Fut.peek fut |> Option.get |> Result.is_error)

let () =
  let fut, prom = Fut.make () in
  assert (not @@ Fut.is_resolved fut);
  Fut.fulfill prom (Ok 42);
  assert (Fut.peek fut = Some (Ok 42));
  try
    Fut.fulfill prom (Ok 1);
    assert false
  with _ -> ()

let () =
  let futs = Array.init 10 (fun i -> Fut.spawn ~on:pool (fun () -> i)) in
  Array.iteri (fun i fut -> assert (Fut.wait_block_exn fut = i)) futs

let () =
  let fut =
    Array.init 10 (fun i -> Fut.spawn ~on:pool (fun () -> i)) |> Fut.join_array
  in
  assert (Array.init 10 (fun i -> i) = Fut.wait_block_exn fut)

let () =
  let fut =
    Array.init 10 (fun i ->
        Fut.spawn ~on:pool (fun () ->
            if i < 9 then
              i
            else
              raise Not_found))
    |> Fut.join_array
  in
  (* must fail *)
  assert (Fut.wait_block fut |> Result.is_error)

let mk_ret_delay ?(on = pool) n x =
  Fut.spawn ~on (fun () ->
      Thread.delay n;
      x)

let () =
  let f1 = mk_ret_delay 0.01 1 in
  let f2 = mk_ret_delay 0.01 2 in
  let fut = Fut.both f1 f2 in
  assert (Fut.wait_block_exn fut = (1, 2))

let () =
  let f1 =
    Fut.spawn ~on:pool (fun () ->
        Thread.delay 0.01;
        1)
  in
  let f2 =
    Fut.spawn ~on:pool (fun () ->
        Thread.delay 0.01;
        raise Exit)
  in
  let fut = Fut.both f1 f2 in
  assert (Fut.wait_block fut |> Result.is_error)

let () =
  let f1 =
    Fut.spawn ~on:pool (fun () ->
        Thread.delay 0.01;
        1)
    |> Fut.map ~on:pool2 ~f:(fun x -> x + 1)
  and f2 =
    Fut.spawn ~on:pool (fun () ->
        Thread.delay 0.01;
        10)
    |> Fut.bind ~on:pool2 ~f:(fun x ->
           Fut.spawn ~on:pool (fun () ->
               Thread.delay 0.01;
               x * 2))
  in
  let fut = Fut.both f1 f2 in
  assert (Fut.wait_block fut = Ok (2, 20))

let () =
  let l = [] in
  let l' = Fut.wait_block_exn (Fut.join_list l) in
  assert (l' = [])

let () =
  let l = [] in
  Fut.wait_block_exn (Fut.wait_list l)

let () =
  let f1 = mk_ret_delay 0.01 true in
  let f2 = mk_ret_delay 0.9 false in
  let fut = Fut.choose f1 f2 in
  assert (Fut.wait_block fut = Ok (Either.Left true))

let () =
  let f1 = mk_ret_delay 0.9 "foo" in
  let f2 = mk_ret_delay 0.01 "bar" in
  let fut = Fut.choose_same f1 f2 in
  assert (Fut.wait_block fut = Ok "bar")

let () =
  let n = Atomic.make 0 in
  let fut1 =
    Fut.for_ ~on:pool 100 (fun i -> ignore (Atomic.fetch_and_add n i : int))
  and fut2 =
    Fut.for_ ~on:pool 100 (fun i -> ignore (Atomic.fetch_and_add n i : int))
  in
  ignore (Fut.wait_block_exn (Fut.join_list [ fut1; fut2 ]) : _ list);
  assert (Atomic.get n = 99 * 100)

let () =
  let f1 = mk_ret_delay 0.1 "foo" in
  assert (
    try
      ignore (Fut.get_or_fail f1);
      false
    with Fut.Not_ready -> true);
  ignore (Fut.wait_block f1);
  assert (Fut.get_or_fail f1 = Ok "foo")

let () =
  let x = [| Fut.return 1 |] |> Fut.join_array |> Fut.wait_block_exn in
  assert (x = [| 1 |])

let () =
  let run_for n =
    let l = List.init n (fun x -> x) in
    let sum = Atomic.make 0 in
    Fut.for_list ~on:pool l (fun x -> ignore (Atomic.fetch_and_add sum x : int))
    |> Fut.wait_block_exn;
    assert (Atomic.get sum = List.fold_left ( + ) 0 l)
  in

  List.iter run_for [ 1; 10; 50; 1_000 ]
