(** Fork-join primitives.

    @since 0.3 *)

val both : (unit -> 'a) -> (unit -> 'b) -> 'a * 'b
(** [both f g] runs [f()] and [g()], potentially in parallel,
    and returns their result when both are done.
    If any of [f()] and [g()] fails, then the whole computation fails.

    This must be run from within the pool: for example, inside {!Pool.run}
    or inside a {!Fut.spawn} computation.
    This is because it relies on an effect handler to be installed.

    @since 0.3
    {b NOTE} this is only available on OCaml 5. *)
