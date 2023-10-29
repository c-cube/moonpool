let start_thread_on_some_domain f x =
  let did = Random.int (D_pool_.n_domains ()) in
  D_pool_.run_on_and_wait did (fun () -> Thread.create f x)

let recommended_thread_count () = Domain_.recommended_number ()
let spawn = Fut.spawn

module Atomic = Atomic_
module Blocking_queue = Bb_queue
module Bounded_queue = Bounded_queue
module Chan = Chan
module Fifo_pool = Fifo_pool
module Fork_join = Fork_join
module Fut = Fut
module Lock = Lock
module Immediate_runner = Immediate_runner
module Pool = Fifo_pool
module Runner = Runner
module Thread_local_storage = Thread_local_storage_
module Ws_pool = Ws_pool

module Private = struct
  module Ws_deque_ = Ws_deque_
  module Suspend_ = Suspend_
end
