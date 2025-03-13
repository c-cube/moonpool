(** Main thread.

    This is evolved from [Moonpool.Immediate_runner], but unlike it,
    this API assumes you run it in a thread (possibly
    the main thread) which will block until the initial computation is done.

    This means it's reasonable to use [Main.main (fun () -> do_everything)]
    at the beginning of the program.
    Other Moonpool pools can be created for background tasks, etc. to do the
    heavy lifting, and the main thread (inside this immediate runner) can coordinate
    tasks via [Fiber.await].

    Aside from the fact that this blocks the caller thread, it is fairly similar to
    {!Background_thread} in that there's a single worker to process
    tasks/fibers.

    This handles effects, including the ones in {!Fiber}.

    @since 0.6
*)

val main : (Moonpool.Runner.t -> 'a) -> 'a
(** [main f] runs [f()] in a scope that handles effects, including {!Fiber.await}.

    This scope can run background tasks as well, in a cooperative fashion. *)

val main' : ?block_signals:bool -> unit -> (Moonpool.Runner.t -> 'a) -> 'a
(** Same as {!main} but with room for optional arguments.
    @since NEXT_RELEASE *)
