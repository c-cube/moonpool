let enabled () = false
let dummy_span = 0L
let message = ignore
let enter_span _name = dummy_span
let exit_span = ignore
let set_thread_name = ignore
let with_span _ f = f dummy_span
