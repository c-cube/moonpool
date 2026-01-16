type span

val dummy_span : span
val enter_span : string -> span
val exit_span : span -> unit
val with_span : string -> (span -> 'a) -> 'a
val enabled : unit -> bool
val set_thread_name : string -> unit
