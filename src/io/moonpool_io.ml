module Fd = Picos_io_fd
module Unix = Picos_io.Unix
module Select = Picos_io_select

let fd_of_unix_fd : Unix.file_descr -> Fd.t = Fun.id
let configure = Select.configure

(** {2 Async read/write} *)

let read = Unix.read
let write = Unix.write
