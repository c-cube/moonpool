(** Utils for pools *)

val num_threads : ?num_threads:int -> unit -> int
(** Number of threads a pool should have.
    @param num_threads user-specified number of threads *)

val on_exn : exn -> Printexc.raw_backtrace -> unit
(** Default exn handler *)

val default_thread_init_exit_ : dom_id:int -> t_id:int -> unit -> unit
(** No-op default for [on_init_thread]/[on_exit_thread] *)

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?num_threads:int ->
  ?name:string ->
  'a
(** Shared shape of the [create] argument list used by pool implementations. *)

val spawn_workers_round_robin :
  num_threads:int -> (int -> dom_id:int -> Thread.t) -> Thread.t array
(** [spawn_workers_round_robin ~num_threads mk_thread] spawns [num_threads]
    worker threads across domains. Calls [mk_thread] to create [num_threads]
    threads (to start workers). *)
