let time_s = Unix.gettimeofday
let[@inline] time_ns () = Int64.of_float (floor (time_s () *. 1e9))
