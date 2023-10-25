(** A simple thread pool.

    This uses a single blocking queue to manage tasks, it's very
    simple and reliable. Like {!Pool} it distributes a fixed number
    of workers over several domains.

    @since NEXT_RELEASE *)

include module type of Runner

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?min:int ->
  ?per_domain:int ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

val create : (unit -> t, _) create_args
(** [create ()] makes a new thread pool.
     @param on_init_thread called at the beginning of each new thread in the pool.
     @param min minimum size of the pool. See {!Pool.create_args}.
     @param per_domain is the number of threads allocated per domain in the fixed
       domain pool. See {!Pool.create_args}.
     @param on_exit_thread called at the end of each worker thread in the pool.
     @param around_task a pair of [before, after] functions
     ran around each task. See {!Pool.create_args}.
  *)

val with_ : (unit -> (t -> 'a) -> 'a, _) create_args
(** [with_ () f] calls [f pool], where [pool] is obtained via {!create}.
    When [f pool] returns or fails, [pool] is shutdown and its resources
    are released.
    Most parameters are the same as in {!create}. *)
