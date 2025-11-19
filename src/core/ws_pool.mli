(** Work-stealing thread pool.

    A pool of threads with a worker-stealing scheduler. The pool contains a
    fixed number of worker threads that wait for work items to come, process
    these, and loop.

    This is good for CPU-intensive tasks that feature a lot of small tasks. Note
    that tasks will not always be processed in the order they are scheduled, so
    this is not great for workloads where the latency of individual tasks matter
    (for that see {!Fifo_pool}).

    This implements {!Runner.t} since 0.3.

    If a pool is no longer needed, {!shutdown} can be used to signal all threads
    in it to stop (after they finish their work), and wait for them to stop.

    The threads are distributed across a fixed domain pool (whose size is
    determined by {!Domain.recommended_domain_count}. See {!create} for more
    details. *)

include module type of Runner

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?num_threads:int ->
  ?name:string ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

val create : (unit -> t, _) create_args
(** [create ()] makes a new thread pool.
    @param on_init_thread
      called at the beginning of each new thread in the pool.
    @param num_threads
      size of the pool, ie. number of worker threads. It will be at least [1]
      internally, so [0] or negative values make no sense. The default is
      [Domain.recommended_domain_count()], ie one worker thread per CPU core.

    Note that specifying [num_threads=n] means that the degree of parallelism is
    at most [n]. This behavior is different than the one of [Domainslib], see
    https://github.com/c-cube/moonpool/issues/41 for context.

    If you want to use all cores, use [Domain.recommended_domain_count()].

    @param on_exit_thread called at the end of each thread in the pool
    @param name
      a name for this thread pool, used if tracing is enabled (since 0.6) *)

val with_ : (unit -> (t -> 'a) -> 'a, _) create_args
(** [with_ () f] calls [f pool], where [pool] is obtained via {!create}. When
    [f pool] returns or fails, [pool] is shutdown and its resources are
    released.

    Most parameters are the same as in {!create}.
    @since 0.3 *)
