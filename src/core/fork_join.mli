(** Fork-join primitives.

    {b NOTE} These are only available on OCaml 5.0 and above.

    @since 0.3 *)

[@@@ifge 5.0]

val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
(** [both f g] runs [f()] and [g()], potentially in parallel,
    and returns their result when both are done.
    If any of [f()] and [g()] fails, then the whole computation fails.

    This must be run from within the pool: for example, inside {!Pool.run}
    or inside a {!Fut.spawn} computation.
    This is because it relies on an effect handler to be installed.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val both_ignore : (unit -> _) -> (unit -> _) -> unit
(** Same as [both f g |> ignore].
    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val for_ : ?chunk_size:int -> int -> (int -> int -> unit) -> unit
(** [for_ n f] is the parallel version of [for i=0 to n-1 do f i done].

    [f] is called with parameters [low] and [high] and must use them like so:
    {[ for j = low to high do (* … actual work *) done ]}.
    If [chunk_size=1] then [low=high] and the loop is not actually needed.

    @param chunk_size controls the granularity of parallelism.
      The default chunk size is not specified.
    See {!all_array} or {!all_list} for more details.

    Example:
    {[
      let total_sum = Atomic.make 0

      let() = for_ ~chunk_size:5 100
        (fun low high ->
          (* iterate on the range sequentially. The range should have 5 items or less. *)
          let local_sum = ref 0 in
          for j=low to high do
            local_sum := !local_sum + j
          done;
          ignore (Atomic.fetch_and_add total_sum !local_sum : int)))

      let() = assert (Atomic.get total_sum = 4950)
    ]}

    Note how we still compute a local sum sequentially in [(fun low high -> …)],
    before combining it wholesale into [total_sum]. When the chunk size is large,
    this can have a dramatic impact on the synchronization overhead.

    When [chunk_size] is not provided, the library will attempt to guess a value
    that keeps all cores busy but runs as few tasks as possible to reduce
    the synchronization overhead.

    Use [~chunk_size:1] if you explicitly want to
    run each iteration of the loop in its own task.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val all_array : ?chunk_size:int -> (unit -> 'a) array -> 'a array
(** [all_array fs] runs all functions in [fs] in tasks, and waits for
    all the results.

    @param chunk_size if equal to [n], groups items by [n] to be run in
      a single task. Default is [1].

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val all_list : ?chunk_size:int -> (unit -> 'a) list -> 'a list
(** [all_list fs] runs all functions in [fs] in tasks, and waits for
    all the results.

    @param chunk_size if equal to [n], groups items by [n] to be run in
      a single task. Default is not specified.
      This parameter is available since 0.3.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val all_init : ?chunk_size:int -> int -> (int -> 'a) -> 'a list
(** [all_init n f] runs functions [f 0], [f 1], … [f (n-1)] in tasks, and waits for
    all the results.

    @param chunk_size if equal to [n], groups items by [n] to be run in
      a single task. Default is not specified.
      This parameter is available since 0.3.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val map_array : ?chunk_size:int -> ('a -> 'b) -> 'a array -> 'b array
(** [map_array f arr] is like [Array.map f arr], but runs in parallel.
    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val map_list : ?chunk_size:int -> ('a -> 'b) -> 'a list -> 'b list
(** [map_list f l] is like [List.map f l], but runs in parallel.
    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

[@@@endif]
