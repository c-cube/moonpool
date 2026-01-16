type span = unit

let enabled () = false
let dummy_span = ()
let enter_span _name = dummy_span
let exit_span = ignore
let set_thread_name = ignore
let with_span _ f = f dummy_span
