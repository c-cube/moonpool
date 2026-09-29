open! Moonpool

let () = Watchdog.arm ()
let ( let@ ) = ( @@ )

(* a raising callback must not prevent other callbacks from running *)
let () =
  let ran = ref 0 in
  let fut, prom = Fut.make () in
  Fut.on_result fut (fun _ -> incr ran);
  Fut.on_result fut (fun _ -> failwith "bad callback");
  Fut.on_result fut (fun _ -> incr ran);
  Fut.fulfill prom (Ok ());
  assert (!ran = 2)

(* [map ~on] on a shut down runner fails instead of never resolving *)
let () =
  let pool = Ws_pool.create ~num_threads:1 () in
  let fut, prom = Fut.make () in
  let fut2 = Fut.map ~on:pool ~f:(fun x -> x + 1) fut in
  Runner.shutdown pool;
  Fut.fulfill prom (Ok 1);
  match Fut.peek fut2 with
  | Some (Error ebt) -> assert (Exn_bt.exn ebt = Shutdown)
  | _ -> assert false

exception Item_failed

(* for_iter keeps the first item's error when more items come afterwards *)
let () =
  let@ pool = Ws_pool.with_ ~num_threads:4 () in
  for _i = 1 to 200 do
    let it yield =
      yield 0;
      Thread.delay 0.001;
      for j = 1 to 20 do
        yield j
      done
    in
    let fut =
      Fut.for_iter ~on:pool it (fun i -> if i = 0 then raise Item_failed)
    in
    match Fut.wait_block fut with
    | Error ebt -> assert (Exn_bt.exn ebt = Item_failed)
    | Ok () -> assert false
  done
