include Runner

let ( let@ ) = ( @@ )

type ('a, 'b) create_args =
  ?on_init_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exit_thread:(dom_id:int -> t_id:int -> unit -> unit) ->
  ?on_exn:(exn -> Printexc.raw_backtrace -> unit) ->
  ?name:string ->
  'a
(** Arguments used in {!create}. See {!create} for explanations. *)

let create ?on_init_thread ?on_exit_thread ?on_exn ?name () : t =
  Fifo_pool.create ?on_init_thread ?on_exit_thread ?on_exn ?name ~num_threads:1
    ()

let with_ ?on_init_thread ?on_exit_thread ?on_exn ?name () f =
  let pool = create ?on_init_thread ?on_exit_thread ?on_exn ?name () in
  let@ () = Fun.protect ~finally:(fun () -> shutdown pool) in
  f pool
