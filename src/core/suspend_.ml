type suspension = unit Exn_bt.result -> unit
type task = unit -> unit

type suspension_handler = {
  handle:
    run:(task -> unit) ->
    resume:(suspension -> unit Exn_bt.result -> unit) ->
    suspension ->
    unit;
}
[@@unboxed]

type with_suspend_handler =
  | WSH : {
      on_suspend: unit -> 'state;
          (** on_suspend called when [f()] suspends itself. *)
      run: 'state -> task -> unit;  (** run used to schedule new tasks *)
      resume: 'state -> suspension -> unit Exn_bt.result -> unit;
          (** resume run the suspension. Must be called exactly once. *)
    }
      -> with_suspend_handler

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

module A = Atomic_

type _ Effect.t +=
  | Suspend : suspension_handler -> unit Effect.t
  | Yield : unit Effect.t

let[@inline] yield () = Effect.perform Yield
let[@inline] suspend h = Effect.perform (Suspend h)

let with_suspend (WSH { on_suspend; run; resume }) (f : unit -> unit) : unit =
  let module E = Effect.Deep in
  (* effect handler *)
  let effc : type e. e Effect.t -> ((e, _) E.continuation -> _) option =
    function
    | Suspend h ->
      (* TODO: discontinue [k] if current fiber (if any) is cancelled? *)
      Some
        (fun k ->
          let state = on_suspend () in
          let k' : suspension = function
            | Ok () -> E.continue k ()
            | Error ebt -> Exn_bt.discontinue k ebt
          in
          h.handle ~run:(run state) ~resume:(resume state) k')
    | Yield ->
      (* TODO: discontinue [k] if current fiber (if any) is cancelled? *)
      Some
        (fun k ->
          let state = on_suspend () in
          let k' : suspension = function
            | Ok () -> E.continue k ()
            | Error ebt -> Exn_bt.discontinue k ebt
          in
          resume state k' @@ Ok ())
    | _ -> None
  in

  E.try_with f () { E.effc }

[@@@ocaml.alert "+unstable"]
[@@@else_]

let[@inline] with_suspend (WSH _) f = f ()

[@@@endif]
