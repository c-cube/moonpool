module Fd = Picos_io_fd
module Unix = Picos_io.Unix
module Select = Picos_io_select

let fd_of_unix_fd : Unix.file_descr -> Fd.t = Fun.id

open struct
  let has_configured = Atomic.make false
end

let configure ?intr_sig ?handle_sigchld ?ignore_sigpipe () : unit =
  if not (Atomic.exchange has_configured true) then
    Select.configure ?intr_sig ?handle_sigchld ?ignore_sigpipe ()

(** [main f] runs [f()] inside a scheduler. *)
let main ?intr_sig ?handle_sigchld ?ignore_sigpipe (f : Moonpool.Runner.t -> 'a)
    : 'a =
  configure ?intr_sig ?handle_sigchld ?ignore_sigpipe ();
  Moonpool_fib.main f

(** {2 Async read/write} *)

let read = Unix.read
let write = Unix.write
