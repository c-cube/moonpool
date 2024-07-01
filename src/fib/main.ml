exception Oh_no of Exn_bt.t

let main (f : Runner.t -> 'a) : 'a =
  let st = Fifo_pool.Private_.create_state ~threads:[| Thread.self () |] () in
  let runner = Fifo_pool.Private_.runner_of_state st in
  try
    let fiber = Fiber.spawn_top ~on:runner (fun () -> f runner) in
    Fiber.on_result fiber (fun _ -> Runner.shutdown_without_waiting runner);
    (* run the main thread *)
    Fifo_pool.Private_.run_thread st runner ~on_exn:(fun e bt ->
        let ebt = Exn_bt.make e bt in
        Fiber.Private_.cancel_from_outside fiber ebt;
        raise (Oh_no ebt));
    match Fiber.peek fiber with
    | Some (Ok x) -> x
    | Some (Error ebt) -> Exn_bt.raise ebt
    | None -> assert false
  with Oh_no ebt -> Exn_bt.raise ebt
