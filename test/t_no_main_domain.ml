open! Moonpool

let () = Watchdog.arm ()
let n_domains = Private.num_domains ()

let check_domains name create =
  let n = n_domains in
  let doms = Blocking_queue.create () in
  let pool =
    create ~num_threads:n ~on_init_thread:(fun ~dom_id ~t_id:_ () ->
        Blocking_queue.push doms dom_id)
  in
  for _i = 1 to n do
    let dom_id = Blocking_queue.pop doms in
    if n_domains > 1 && dom_id = 0 then
      failwith (Printf.sprintf "%s: worker on the main domain" name)
  done;
  Runner.shutdown pool

let () =
  check_domains "ws_pool" (fun ~num_threads ~on_init_thread ->
      Ws_pool.create ~use_main_domain:false ~num_threads ~on_init_thread ());
  check_domains "fifo_pool" (fun ~num_threads ~on_init_thread ->
      Fifo_pool.create ~use_main_domain:false ~num_threads ~on_init_thread ())

let () =
  let expected = max 1 (n_domains - 1) in
  let pool = Ws_pool.create ~use_main_domain:false () in
  assert (Runner.size pool = expected);
  Runner.shutdown pool;
  let pool = Fifo_pool.create ~use_main_domain:false () in
  assert (Runner.size pool = expected);
  Runner.shutdown pool
