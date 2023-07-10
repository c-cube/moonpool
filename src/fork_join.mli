(** Fork-join primitives.

    {b NOTE} These are only available on OCaml 5.0 and above.

    @since 0.3 *)

[@@@ifge 5.0]

type 'a iter = ('a -> unit) -> unit
(** Iterators of type ['a].
    @since NEXT_RELEASE *)

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

val for_ : ?chunk_size:int -> int -> (int iter -> unit) -> unit
(** [for_ n f] is the parallel version of [for i=0 to n-1 do f i done].

    [f] is called with a [range] parameter, which is an iterator on indices
    [f] should process.

    @param chunk_size controls the granularity of parallelism.
      The default chunk size is not specified.
    See {!all_array} or {!all_list} for more details.

    Example:
    {[
      let total_sum = Atomic.make 0

      let() = for_ ~chunk_size:5 100
        (fun range ->
          (* iterate on the range sequentially. The range should have 5 items or less. *)
          let local_sum = ref 0 in
          range
            (fun i -> local_sum := !local_sum + n);
          ignore (Atomic.fetch_and_add total_sum !local_sum : int)))

      let() = assert (Atomic.get total_sum = 4950)
    ]}

    @since NEXT_RELEASE
    {b NOTE} this is only available on OCaml 5. *)

val all_array : ?chunk_size:int -> (unit -> 'a) array -> 'a array
(** [all_array fs] runs all functions in [fs] in tasks, and waits for
    all the results.

    @param chunk_size if equal to [n], groups items by [n] to be run in
      a single task. Default is [1].

    @since NEXT_RELEASE
    {b NOTE} this is only available on OCaml 5. *)

val all_list : ?chunk_size:int -> (unit -> 'a) list -> 'a list
(** [all_list fs] runs all functions in [fs] in tasks, and waits for
    all the results.

    @param chunk_size if equal to [n], groups items by [n] to be run in
      a single task. Default is not specified.
      This parameter is available since NEXT_RELEASE.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val all_init : ?chunk_size:int -> int -> (int -> 'a) -> 'a list
(** [all_init n f] runs functions [f 0], [f 1], … [f (n-1)] in tasks, and waits for
    all the results.

    @param chunk_size if equal to [n], groups items by [n] to be run in
      a single task. Default is not specified.
      This parameter is available since NEXT_RELEASE.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

type 'a commutative_monoid = {
  neutral: unit -> 'a;  (** Neutral element *)
  combine: 'a -> 'a -> 'a;  (** Combine two items. *)
}
(** A commutative monoid: order of operations does not matter.
    @since NEXT_RELEASE *)

val map_reduce_commutative :
  ?chunk_size:int ->
  gen:(int -> 'a) ->
  map:('a -> 'b) ->
  reduce:'b commutative_monoid ->
  int ->
  'b
(** [map_reduce_commutative ~gen ~map ~reduce n] produces items of type ['a]
    using [gen 0], [gen 1], …, [gen (n-1)]. Items are then mapped using [map]
    in background tasks (each task processes up to [chunk_size] items at a time).

    Then, items of type ['b] obtained by mapping are reduced together using the
    definition of the commutative monoid [reduce]. The order in which they
    are reduced is not specified.

    @param chunk_size controls granularity of the mapping process
    @param gen generates items to process based on an index
    @param map takes an item and processes it, independently of other items
    @param reduce is used to aggregate results of mapping.

    @since NEXT_RELEASE
    {b NOTE} this is only available on OCaml 5.
*)

[@@@endif]
