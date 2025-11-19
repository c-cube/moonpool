(** A simple thread pool in FIFO order.

    FIFO: first-in, first-out. Basically tasks are put into a queue, and worker
    threads pull them out of the queue at the other end.

    Since this uses a single blocking queue to manage tasks, it's very simple
    and reliable. The number of worker threads is fixed, but they are spread
    over several domains to enable parallelism.

    This can be useful for latency-sensitive applications (e.g. as a pool of
    workers for network servers). Work-stealing pools might have higher
    throughput but they're very unfair to some tasks; by contrast, here, older
    tasks have priority over younger tasks.

    @since 0.5 *)

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
      number of worker threads. See {!Ws_pool.create} for more details.
    @param on_exit_thread called at the end of each worker thread in the pool.
    @param name name for the pool, used in tracing (since 0.6) *)

val with_ : (unit -> (t -> 'a) -> 'a, _) create_args
(** [with_ () f] calls [f pool], where [pool] is obtained via {!create}. When
    [f pool] returns or fails, [pool] is shutdown and its resources are
    released. Most parameters are the same as in {!create}. *)

(**/**)

module Private_ : sig
  type worker_state

  val worker_ops : worker_state Worker_loop_.ops

  val create_single_threaded_state :
    thread:Thread.t ->
    ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
    unit ->
    worker_state

  val runner_of_state : worker_state -> Runner.t
end

(**/**)
