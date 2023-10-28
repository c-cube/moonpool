(* exhibits unfairness *)

open Moonpool

let ( let@ ) = ( @@ )

let sleep_for f () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "sleep" in
  Thread.delay f

let run ~kind () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "run" in

  let pool =
    let on_init_thread ~dom_id:_ ~t_id () =
      Trace.set_thread_name (Printf.sprintf "pool worker %d" t_id)
    and around_task =
      ( (fun self -> Trace.counter_int "n_tasks" (Ws_pool.num_tasks self)),
        fun self () -> Trace.counter_int "n_tasks" (Ws_pool.num_tasks self) )
    in

    match kind with
    | `Simple -> Fifo_pool.create ~num_threads:3 ~on_init_thread ~around_task ()
    | `Ws_pool -> Ws_pool.create ~num_threads:3 ~on_init_thread ~around_task ()
  in

  (* make all threads busy *)
  Ws_pool.run_async pool (sleep_for 0.01);
  Ws_pool.run_async pool (sleep_for 0.01);
  Ws_pool.run_async pool (sleep_for 0.05);

  let t = Unix.gettimeofday () in
  for _i = 1 to 100 do
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "schedule step" in
    Ws_pool.run_async pool (sleep_for 0.001);
    Ws_pool.run_async pool (sleep_for 0.001);
    Ws_pool.run_async pool (sleep_for 0.01)
  done;

  Printf.printf "pool size: %d\n%!" (Ws_pool.num_tasks pool);
  (let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "shutdown" in
   Ws_pool.shutdown pool);
  Printf.printf "pool size after shutdown: %d\n%!" (Ws_pool.num_tasks pool);

  let elapsed = Unix.gettimeofday () -. t in
  Printf.printf "elapsed: %.4fs\n%!" elapsed

let () =
  let@ () = Trace_tef.with_setup () in
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main" in
  run ~kind:`Simple ();
  run ~kind:`Ws_pool ()
