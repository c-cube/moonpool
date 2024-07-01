module Trace = Trace_core

let enabled = Trace.enabled
let dummy_span = Int64.min_int
let dummy_file_ = "<unknown file>"
let set_thread_name = Trace.set_thread_name
let[@inline] message msg = Trace.message msg

let[@inline] enter_span name : int64 =
  if name = "" then
    dummy_span
  else
    Trace.enter_span ~__FILE__:dummy_file_ ~__LINE__:0 name

let[@inline] exit_span sp = if sp <> dummy_span then Trace.exit_span sp

let with_span name f =
  let sp = enter_span name in
  try
    let x = f sp in
    exit_span sp;
    x
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    exit_span sp;
    Printexc.raise_with_backtrace exn bt
