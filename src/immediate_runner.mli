(** Runner that runs tasks immediately in the caller thread.

    Whenever a task is submitted to this runner via [Runner.run_async r task],
    the task is run immediately in the caller thread as [task()].
    There are no background threads, no resource, this is just a trivial
    implementation of the interface.

    This can be useful when an implementation needs a runner, but there isn't
    enough work to justify starting an actual full thread pool.

    Another situation is when threads cannot be used at all (e.g. because you
    plan to call [Unix.fork] later).

    @since NEXT_RELEASE
*)

include module type of Runner

val runner : t
(** The trivial runner that actually runs tasks at the calling point. *)
