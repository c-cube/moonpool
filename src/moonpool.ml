let start_thread_on_some_domain f x =
  let did = Random.int (D_pool_.n_domains ()) in
  D_pool_.run_on_and_wait did (fun () -> Thread.create f x)

module Pool = Pool
module Fut = Fut
module Blocking_queue = Bb_queue
module Atomic = Atomic_
