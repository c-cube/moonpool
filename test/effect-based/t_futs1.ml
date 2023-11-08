[@@@ifge 5.0]

open! Moonpool

let pool = Ws_pool.create ~num_threads:4 ()

let () =
  let fut = Array.init 10 (fun i -> Fut.spawn ~on:pool (fun () -> i)) in
  let fut2 = Fut.spawn ~on:pool (fun () -> Array.map Fut.await fut) in
  assert (Fut.wait_block fut2 = Ok (Array.init 10 (fun x -> x)))

let () =
  let fut =
    Array.init 10 (fun i ->
        Fut.spawn ~on:pool (fun () ->
            if i < 9 then
              i
            else
              raise Not_found))
  in
  let fut2 = Fut.spawn ~on:pool (fun () -> Array.map Fut.await fut) in
  (* must fail *)
  assert (Fut.wait_block fut2 |> Result.is_error)

let mk_ret_delay ?(on = pool) n x =
  Fut.spawn ~on (fun () ->
      Thread.delay n;
      x)

let () =
  let f1 = mk_ret_delay 0.01 1 in
  let f2 = mk_ret_delay 0.01 2 in
  let fut = Fut.spawn ~on:pool (fun () -> Fut.await f1, Fut.await f2) in
  assert (Fut.wait_block_exn fut = (1, 2))

let () =
  let f1 =
    let f =
      Fut.spawn ~on:pool (fun () ->
          Thread.delay 0.01;
          1)
    in
    Fut.spawn ~on:pool (fun () -> Fut.await f + 1)
  and f2 =
    let f =
      Fut.spawn ~on:pool (fun () ->
          Thread.delay 0.01;
          10)
    in
    Fut.spawn ~on:pool (fun () ->
        Thread.delay 0.01;
        Fut.await f * 2)
  in
  let fut = Fut.both f1 f2 in
  assert (Fut.wait_block fut = Ok (2, 20))

[@@@endif]
