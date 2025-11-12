exception Oh_no of Exn_bt.t

let main' ?(block_signals = false) () (f : Runner.t -> 'a) : 'a =
  let module WL = Worker_loop_ in
  let worker_st =
    Fifo_pool.Private_.create_single_threaded_state ~thread:(Thread.self ())
      ~on_exn:(fun e bt -> raise (Oh_no (Exn_bt.make e bt)))
      ()
  in
  let runner = Fifo_pool.Private_.runner_of_state worker_st in
  try
    let fut = Fut.spawn ~on:runner (fun () -> f runner) in
    Fut.on_result fut (fun _ -> Runner.shutdown_without_waiting runner);

    Thread_local_storage.set Runner.For_runner_implementors.k_cur_st
      { cur_fiber = Picos.Fiber.create ~forbid:true fut; runner };

    (* run the main thread *)
    WL.worker_loop worker_st ~block_signals (* do not disturb existing thread *)
      ~ops:Fifo_pool.Private_.worker_ops;

    match Fut.peek fut with
    | Some (Ok x) -> x
    | Some (Error ebt) -> Exn_bt.raise ebt
    | None -> assert false
  with Oh_no ebt -> Exn_bt.raise ebt

let main f =
  main' () f ~block_signals:false (* do not disturb existing thread *)
