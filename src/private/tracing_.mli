val dummy_span : int64
val enter_span : string -> int64
val exit_span : int64 -> unit
val with_span : string -> (int64 -> 'a) -> 'a
val message : string -> unit
val enabled : unit -> bool
val set_thread_name : string -> unit
