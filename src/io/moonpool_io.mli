(** Simple event loop based on {!Unix.select}.

    This library combines {!Fuseau}'s fibers with a simple
    event loop for IOs based on {!Unix.select}.
    It's useful for simple situations or portability.
    For bigger system it's probably better to use another
    event loop. *)

include module type of struct
  include Moonpool_fib
end

module IO_unix = IO_unix
module Net = Net
module Timer = Timer

(* FIXME: lazy background thread for the poll loop? *)

val main : (unit -> 'a) -> 'a
(** A version of {!Moonpool_fib.main} that uses a Unix-based event loop. *)
