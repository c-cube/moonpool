type suspension = (unit, exn * Printexc.raw_backtrace) result -> unit
type task = unit -> unit

type suspension_handler = {
  handle: run:(with_handler:bool -> task -> unit) -> suspension -> unit;
}
[@@unboxed]

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

type _ Effect.t += Suspend : suspension_handler -> unit Effect.t

let[@inline] suspend h = Effect.perform (Suspend h)

let with_suspend ~(run : with_handler:bool -> task -> unit) (f : unit -> unit) :
    unit =
  let module E = Effect.Deep in
  (* effect handler *)
  let effc : type e. e Effect.t -> ((e, _) E.continuation -> _) option =
    function
    | Suspend h ->
      Some
        (fun k ->
          let k' : suspension = function
            | Ok () -> E.continue k ()
            | Error (exn, bt) -> E.discontinue_with_backtrace k exn bt
          in
          h.handle ~run k')
    | _ -> None
  in

  E.try_with f () { E.effc }

[@@@ocaml.alert "+unstable"]
[@@@else_]

let with_suspend ~run:_ f = f ()

[@@@endif]
