exception Oh_no of Exn_bt.t

let main' ?(block_signals = false) () (f : Runner.t -> 'a) : 'a =
  let worker_st =
    Fifo_pool.Private_.create_single_threaded_state ~thread:(Thread.self ())
      ~on_exn:(fun e bt -> raise (Oh_no (Exn_bt.make e bt)))
      ()
  in
  let runner = Fifo_pool.Private_.runner_of_state worker_st in
  try
    let fiber = Fiber.spawn_top ~on:runner (fun () -> f runner) in
    Fiber.on_result fiber (fun _ -> Runner.shutdown_without_waiting runner);

    (* run the main thread *)
    Moonpool.Private.Worker_loop_.worker_loop worker_st
      ~block_signals (* do not disturb existing thread *)
      ~ops:Fifo_pool.Private_.worker_ops;

    match Fiber.peek fiber with
    | Some (Ok x) -> x
    | Some (Error ebt) -> Exn_bt.raise ebt
    | None -> assert false
  with Oh_no ebt -> Exn_bt.raise ebt

let main f =
  main' () f ~block_signals:false (* do not disturb existing thread *)
