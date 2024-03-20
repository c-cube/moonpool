(** A simple runner with a single background thread.

    Because this is guaranteed to have a single worker thread,
    tasks scheduled in this runner always run asynchronously but
    in a sequential fashion.

    This is similar to {!Fifo_pool} with exactly one thread.

    @since 0.6
*)

include module type of Runner

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?around_task:(t -> 'b) * (t -> 'b -> unit) ->
  ?name:string ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

val create : (unit -> t, _) create_args
(** Create the background runner *)

val with_ : (unit -> (t -> 'a) -> 'a, _) create_args
