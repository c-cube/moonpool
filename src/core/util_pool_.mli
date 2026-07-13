(** Utils for pools *)

val num_threads : ?num_threads:int -> unit -> int
(** Number of threads a pool should have.
    @param num_threads user-specified number of threads *)

val on_exn : exn -> Printexc.raw_backtrace -> unit
(** Default exn handler *)
