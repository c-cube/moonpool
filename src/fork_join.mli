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

val all_list : (unit -> 'a) list -> 'a list
(** [all_list fs] runs all functions in [fs] in tasks, and waits for
    all the results.
    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

val all_init : int -> (int -> 'a) -> 'a list
(** [all_init n f] runs functions [f 0], [f 1], … [f (n-1)] in tasks, and waits for
    all the results.
    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)

[@@@endif]
