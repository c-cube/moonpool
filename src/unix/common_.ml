module M = Moonpool
module Exn_bt = M.Exn_bt
module A = Moonpool.Atomic
module Fiber = Moonpool_fib.Fiber
module Tracing_ = Moonpool.Private.Tracing_

let ( let@ ) = ( @@ )
let spf = Printf.sprintf
let _default_buf_size = 4 * 1024
