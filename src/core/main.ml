let main' ?(block_signals = false) () (f : Runner.t -> 'a) : 'a =
  let first_exn : Exn_bt.t option Atomic.t = Atomic.make None in
  let runner = ref Runner.dummy in
  let on_exn e bt =
    if Atomic.compare_and_set first_exn None (Some (Exn_bt.make e bt)) then
      Runner.shutdown_without_waiting !runner
    else
      Util_pool_.on_exn e bt
  in
  let worker_st =
    Fifo_pool.Private_.create_single_threaded_state ~thread:(Thread.self ())
      ~on_exn ()
  in
  runner := Fifo_pool.Private_.runner_of_state worker_st;
  let fut = Fut.spawn ~on:!runner (fun () -> f !runner) in
  Fut.on_result fut (fun _ -> Runner.shutdown_without_waiting !runner);

  (* run the main thread *)
  Worker_loop_.worker_loop worker_st
    ~block_signals (* do not disturb existing thread *)
    ~ops:Fifo_pool.Private_.on_thread_worker_ops;

  Option.iter Exn_bt.raise (Atomic.get first_exn);
  match Fut.peek fut with
  | Some (Ok x) -> x
  | Some (Error ebt) -> Exn_bt.raise ebt
  | None -> assert false

let main f =
  main' () f ~block_signals:false (* do not disturb existing thread *)
