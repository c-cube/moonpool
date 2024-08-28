open Types_

exception Shutdown = Runner.Shutdown

let start_thread_on_some_domain f x =
  let did = Random.int (Domain_pool_.max_number_of_domains ()) in
  Domain_pool_.run_on_and_wait did (fun () -> Thread.create f x)

let run_async = Runner.run_async
let run_wait_block = Runner.run_wait_block
let recommended_thread_count () = Domain_.recommended_number ()
let spawn = Fut.spawn
let spawn_on_current_runner = Fut.spawn_on_current_runner

[@@@ifge 5.0]

let await = Fut.await

[@@@endif]

module Atomic = Atomic_
module Blocking_queue = Bb_queue
module Background_thread = Background_thread
module Bounded_queue = Bounded_queue
module Chan = Chan
module Exn_bt = Exn_bt
module Fifo_pool = Fifo_pool
module Fut = Fut
module Lock = Lock
module Immediate_runner = struct end
module Runner = Runner
module Task_local_storage = Task_local_storage
module Thread_local_storage = Thread_local_storage
module Trigger = Trigger
module Ws_pool = Ws_pool

module Private = struct
  module Ws_deque_ = Ws_deque_
  module Worker_loop_ = Worker_loop_
  module Domain_ = Domain_
  module Tracing_ = Tracing_

  let num_domains = Domain_pool_.max_number_of_domains
end
