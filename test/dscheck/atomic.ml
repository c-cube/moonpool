(* Shadows [Stdlib.Atomic] for [ws_deque_.ml] (copied unmodified from
   [src/private/] by the dune rule in this directory), so it gets compiled
   against DScheck's traced atomics instead of the real ones. *)
include Dscheck.TracedAtomic
