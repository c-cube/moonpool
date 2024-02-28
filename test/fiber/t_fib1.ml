open Moonpool
module F = Moonpool_fib.Fiber

let ( let@ ) = ( @@ )
let runner = Ws_pool.create ~num_threads:8 ()
let lock = Lock.create ()

let logf fmt =
  Printf.ksprintf
    (fun s ->
      let out = stdout in
      (let@ () = Lock.with_ lock in
       output_string out s);
      flush out)
    fmt

let () =
  logf "============\nstart\n";
  let fib =
    F.spawn_top ~on:runner @@ fun () ->
    let subs =
      List.init 5 (fun i ->
          F.spawn_link ~protect:false @@ fun () ->
          Thread.delay 0.000_01;
          i)
    in

    ignore
      (F.spawn_link ~protect:false @@ fun () ->
       Thread.delay 0.2;
       logf "other fib done\n%!"
        : _ F.t);
    logf "wait for subs\n%!";
    List.iteri
      (fun i f ->
        logf "await fiber %d\n%!" i;
        logf "cur fiber is some: %b\n%!"
          (Option.is_some @@ F.Private_.get_cur ());
        let res = F.await f in
        logf "cur fiber is some: %b\n%!"
          (Option.is_some @@ F.Private_.get_cur ());
        Thread.delay 0.000_01;
        F.yield ();
        logf "res %d = %d\n%!" i res)
      subs;
    logf "main fiber done\n%!"
  in

  Fut.wait_block_exn @@ F.res fib;
  logf "main fiber exited\n%!";
  ()

let () =
  (* same but now, cancel *)
  logf "============\nstart\n";
  let fib =
    F.spawn_top ~on:runner @@ fun () ->
    let@ () =
      F.with_self_cancel_callback (fun ebt ->
          logf "main fiber cancelled with %s\n%!" @@ Exn_bt.show ebt)
    in

    let subs =
      List.init 10 (fun i ->
          F.spawn_link ~protect:false @@ fun () ->
          let@ () =
            F.with_self_cancel_callback (fun _ ->
                logf "sub-fiber %d was cancelled\n%!" i)
          in
          Thread.delay (float i *. 0.001);
          F.yield ();
          if i = 7 then (
            logf "I'm fiber %d and I'm about to…\n%!" i;
            failwith "oh no!"
          );
          i)
    in

    List.iteri
      (fun i fib ->
        F.on_result fib (function
          | Ok _ -> logf "fiber %d resolved as ok\n%!" i
          | Error _ -> logf "fiber %d resolved as error\n%!" i))
      subs;

    ignore
      (F.spawn_link ~protect:false @@ fun () ->
       Thread.delay 0.2;
       logf "other fib done\n%!"
        : _ F.t);

    logf "wait for subs\n%!";
    List.iteri
      (fun i f ->
        logf "await fiber %d\n%!" i;
        let res = F.await f in
        logf "res %d = %d\n%!" i res)
      subs;
    logf "yield\n%!";
    F.yield ();
    logf "yielded\n%!";
    logf "main fiber done\n%!"
  in

  F.on_result fib (function
    | Ok () -> logf "main fiber result: ok\n%!"
    | Error ebt -> logf "main fiber result: error %s\n%!" (Exn_bt.show ebt));

  (try Fut.wait_block_exn @@ F.res fib
   with Failure msg -> logf "main fib failed with %S\n%!" msg);
  logf "main fiber exited\n%!";
  ()
