let start_thread_on_some_domain f x =
  let did = Random.int (D_pool_.n_domains ()) in
  D_pool_.run_on_and_wait did (fun () -> Thread.create f x)

module Atomic = Atomic_
module Blocking_queue = Bb_queue
module Chan = Chan
module Fork_join = Fork_join
module Fut = Fut
module Lock = Lock
module Pool = Pool
module Runner = Runner
module Suspend_ = Suspend_
