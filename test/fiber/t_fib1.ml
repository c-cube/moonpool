open! Moonpool
module A = Atomic
module F = Moonpool_fib.Fiber

let ( let@ ) = ( @@ )
let runner = Ws_pool.create ~num_threads:8 ()

module TS = struct
  type t = int list

  let show (s : t) = String.concat "." @@ List.map string_of_int s
  let init = [ 0 ]

  let next_ = function
    | [] -> [ 0 ]
    | n :: tl -> (n + 1) :: tl

  let tick (t : t ref) = t := next_ !t

  let tick_get t =
    tick t;
    !t
end

(* more deterministic logging of events *)
module Log_ = struct
  let events : (TS.t * string) list A.t = A.make []

  let add_event t msg : unit =
    while
      let old = A.get events in
      not (A.compare_and_set events old ((t, msg) :: old))
    do
      ()
    done

  let logf t fmt = Printf.ksprintf (add_event t) fmt

  let print_and_clear () =
    let l =
      A.exchange events []
      |> List.map (fun (ts, msg) -> List.rev ts, msg)
      |> List.sort Stdlib.compare
    in
    List.iter (fun (ts, msg) -> Printf.printf "%s: %s\n" (TS.show ts) msg) l
end

let logf = Log_.logf

let () =
  Printf.printf "============\nstart\n";
  let clock = ref TS.init in
  let fib =
    F.spawn_top ~on:runner @@ fun () ->
    let subs =
      List.init 5 (fun i ->
          F.spawn ~protect:false @@ fun _n ->
          Thread.delay (float i *. 0.01);
          i)
    in

    F.spawn_ignore ~protect:false (fun _n ->
        Thread.delay 0.4;
        TS.tick clock;
        logf !clock "other fib done");

    logf (TS.tick_get clock) "wait for subs";
    List.iteri
      (fun i f ->
        let clock = ref (0 :: i :: !clock) in
        logf !clock "await fiber %d" i;
        logf (TS.tick_get clock) "cur fiber[%d] is some: %b" i
          (Option.is_some @@ F.Private_.get_cur ());
        let res = F.await f in
        logf (TS.tick_get clock) "cur fiber[%d] is some: %b" i
          (Option.is_some @@ F.Private_.get_cur ());
        F.yield ();
        logf (TS.tick_get clock) "res %d = %d" i res)
      subs;
    logf (TS.tick_get clock) "main fiber done"
  in

  Fut.wait_block_exn @@ F.res fib;
  logf (TS.tick_get clock) "main fiber exited";
  Log_.print_and_clear ();
  ()

let () =
  (* same but now, cancel one of the sub-fibers *)
  Printf.printf "============\nstart\n";

  let clock = ref TS.init in
  let fib =
    F.spawn_top ~on:runner @@ fun () ->
    let@ () =
      F.with_self_cancel_callback (fun ebt ->
          logf (TS.tick_get clock) "main fiber cancelled with %s"
          @@ Exn_bt.show ebt)
    in

    logf (TS.tick_get clock) "start fibers";
    let subs =
      List.init 10 (fun i ->
          let clock = ref (0 :: i :: !clock) in
          F.spawn ~protect:false @@ fun _n ->
          let@ () =
            F.with_self_cancel_callback (fun _ ->
                logf (TS.tick_get clock) "sub-fiber %d was cancelled" i)
          in
          Thread.delay (float i *. 0.001);
          F.yield ();
          if i = 7 then (
            logf (TS.tick_get clock) "I'm fiber %d and I'm about to…" i;
            failwith "oh no!"
          );
          i)
    in

    let post = TS.tick_get clock in
    List.iteri
      (fun i fib ->
        F.on_result fib (function
          | Ok _ -> logf (i :: post) "fiber %d resolved as ok" i
          | Error _ -> logf (i :: post) "fiber %d resolved as error" i))
      subs;

    F.spawn_ignore ~protect:false (fun _n ->
        Thread.delay 0.2;
        logf (TS.tick_get clock) "other fib done");

    logf (TS.tick_get clock) "wait for subs";
    List.iteri
      (fun i f ->
        logf (TS.tick_get clock) "await fiber %d" i;
        let res = F.await f in
        logf (TS.tick_get clock) "res %d = %d" i res)
      subs;
    logf (TS.tick_get clock) "yield";
    F.yield ();
    logf (TS.tick_get clock) "yielded";
    logf (TS.tick_get clock) "main fiber done"
  in

  F.on_result fib (function
    | Ok () -> logf (TS.tick_get clock) "main fiber result: ok"
    | Error ebt ->
      logf (TS.tick_get clock) "main fiber result: error %s" (Exn_bt.show ebt));

  (try Fut.wait_block_exn @@ F.res fib
   with Failure msg -> logf (TS.tick_get clock) "main fib failed with %S" msg);
  logf (TS.tick_get clock) "main fiber exited";
  Log_.print_and_clear ();
  ()
