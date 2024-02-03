let start_thread_on_some_domain f x =
  let did = Random.int (Domain_pool_.n_domains ()) in
  Domain_pool_.run_on_and_wait did (fun () -> Thread.create f x)

let run_async = Runner.run_async
let recommended_thread_count () = Domain_.recommended_number ()
let spawn = Fut.spawn
let spawn_on_current_runner = Fut.spawn_on_current_runner

[@@@ifge 5.0]

let await = Fut.await

[@@@endif]

module Atomic = Atomic_
module Blocking_queue = Bb_queue
module Bounded_queue = Bounded_queue
module Chan = Chan
module Exn_bt = Exn_bt
module Fifo_pool = Fifo_pool
module Fut = Fut
module Lock = Lock
module Immediate_runner = Immediate_runner
module Runner = Runner
module Task_local_storage = Task_local_storage
module Thread_local_storage = Thread_local_storage_
module Ws_pool = Ws_pool

module Private = struct
  module Ws_deque_ = Ws_deque_
  module Suspend_ = Suspend_
  module Domain_ = Domain_

  let num_domains = Domain_pool_.n_domains
end
