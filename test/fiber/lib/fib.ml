module Chan = Moonpool.Chan
module Exn_bt = Moonpool.Exn_bt
module A = Atomic
module Fut = Moonpool.Fut

let ( let@ ) = ( @@ )

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

let run1 ~runner () =
  Printf.printf "============\nstart\n%!";
  let clock = ref TS.init in
  let fut =
    Fut.spawn ~on:runner @@ fun () ->
    let chan_progress = Chan.create ~max_size:4 () in
    let chans = Array.init 5 (fun _ -> Chan.create ~max_size:4 ()) in

    let subs =
      List.init 5 (fun i ->
          Fut.spawn ~on:runner @@ fun _n ->
          Thread.delay (float i *. 0.01);
          Chan.pop chans.(i);
          Chan.push chan_progress i;
          i)
    in

    logf (TS.tick_get clock) "wait for subs";

    Moonpool.run_async runner (fun () ->
        for i = 0 to 4 do
          Chan.push chans.(i) ();
          let i' = Chan.pop chan_progress in
          assert (i = i')
        done);

    (let clock0 = !clock in
     List.iteri
       (fun i f ->
         let clock = ref (0 :: i :: clock0) in
         logf !clock "await fiber %d" i;
         let res = Fut.await f in
         Fut.yield ();
         logf (TS.tick_get clock) "res %d = %d" i res)
       subs);

    logf (TS.tick_get clock) "main fiber done"
  in

  Fut.await fut;
  logf (TS.tick_get clock) "main fiber exited";
  Log_.print_and_clear ();
  ()

let run2 ~runner () =
  (* same but now, cancel one of the sub-fibers *)
  Printf.printf "============\nstart\n";

  let to_await = ref [] in

  let clock = ref TS.init in
  let fut =
    Fut.spawn ~on:runner @@ fun () ->
    let chans_unblock = Array.init 10 (fun _i -> Chan.create ~max_size:4 ()) in
    let chan_progress = Chan.create ~max_size:4 () in

    logf (TS.tick_get clock) "start fibers";
    let subs =
      let clock0 = !clock in
      List.init 10 (fun i ->
          let clock = ref (0 :: i :: clock0) in
          Fut.spawn ~on:runner @@ fun _n ->
          Thread.delay 0.002;

          (* sync for determinism *)
          Chan.pop chans_unblock.(i);
          Chan.push chan_progress i;

          if i = 7 then (
            logf (TS.tick_get clock) "I'm fiber %d and I'm about to fail…" i;
            failwith "oh no!"
          );

          i)
    in

    let post = TS.tick_get clock in
    List.iteri
      (fun i fib ->
        Fut.on_result fib (function
          | Ok _ -> logf (i :: post) "fiber %d resolved as ok" i
          | Error _ -> logf (i :: post) "fiber %d resolved as error" i))
      subs;

    (* sequentialize the fibers, for determinism *)
    let sender =
      Fut.spawn ~on:runner (fun () ->
          for j = 0 to 9 do
            Chan.push chans_unblock.(j) ();
            let j' = Chan.pop chan_progress in
            assert (j = j')
          done)
    in
    to_await := sender :: !to_await;

    logf (TS.tick_get clock) "wait for subs";
    List.iteri
      (fun i f ->
        logf (TS.tick_get clock) "await fiber %d" i;
        let res = Fut.await f in
        logf (TS.tick_get clock) "res %d = %d" i res)
      subs;
    logf (TS.tick_get clock) "yield";
    Fut.yield ();
    logf (TS.tick_get clock) "yielded";
    logf (TS.tick_get clock) "main fiber done"
  in

  Fut.on_result fut (function
    | Ok () -> logf (TS.tick_get clock) "main fiber result: ok"
    | Error ebt ->
      logf (TS.tick_get clock) "main fiber result: error %s" (Exn_bt.show ebt));

  (try Fut.await fut
   with Failure msg -> logf (TS.tick_get clock) "main fib failed with %S" msg);
  logf (TS.tick_get clock) "main fiber exited";

  List.iter Fut.await !to_await;

  Log_.print_and_clear ();
  ()
