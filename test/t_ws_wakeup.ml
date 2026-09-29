open! Moonpool

let () = Watchdog.arm ()
let ( let@ ) = ( @@ )

(* each outer task pushes a task into its own local queue, then blocks its
   thread until that task runs. Only the other worker can steal it, so it must
   not stay asleep *)
let () =
  let@ pool = Ws_pool.with_ ~num_threads:2 () in
  for _i = 1 to 10_000 do
    Fut.wait_block_exn
    @@ Fut.spawn ~on:pool (fun () ->
        let sem = Semaphore.Binary.make false in
        Runner.run_async pool (fun () -> Semaphore.Binary.release sem);
        Semaphore.Binary.acquire sem)
  done
