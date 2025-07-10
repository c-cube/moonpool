[@@@ocaml.deprecated "use Picos_std_sync or single threaded solutions"]

module Mutex = Picos_std_sync.Mutex
module Condition = Picos_std_sync.Condition
module Lock = Lock
module Event = Event
module Semaphore = Picos_std_sync.Semaphore
module Lazy = Picos_std_sync.Lazy
module Latch = Picos_std_sync.Latch
module Ivar = Picos_std_sync.Ivar
module Stream = Picos_std_sync.Stream
