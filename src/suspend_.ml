type suspension = (unit, exn * Printexc.raw_backtrace) result -> unit
type task = unit -> unit

type suspension_handler = {
  handle: name:string -> run:(name:string -> task -> unit) -> suspension -> unit;
}
[@@unboxed]

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

module A = Atomic_

type _ Effect.t += Suspend : suspension_handler -> unit Effect.t

let[@inline] suspend h = Effect.perform (Suspend h)

let with_suspend ~name ~on_suspend ~(run : name:string -> task -> unit)
    (f : unit -> unit) : unit =
  let module E = Effect.Deep in
  (* effect handler *)
  let effc : type e. e Effect.t -> ((e, _) E.continuation -> _) option =
    function
    | Suspend h ->
      Some
        (fun k ->
          on_suspend ();
          let k' : suspension = function
            | Ok () -> E.continue k ()
            | Error (exn, bt) -> E.discontinue_with_backtrace k exn bt
          in
          h.handle ~name ~run k')
    | _ -> None
  in

  E.try_with f () { E.effc }

(* DLA interop *)
let prepare_for_await () : Dla_.t =
  (* current state *)
  let st : (string * (name:string -> task -> unit) * suspension) option A.t =
    A.make None
  in

  let release () : unit =
    match A.exchange st None with
    | None -> ()
    | Some (name, run, k) -> run ~name (fun () -> k (Ok ()))
  and await () : unit =
    suspend { handle = (fun ~name ~run k -> A.set st (Some (name, run, k))) }
  in

  let t = { Dla_.release; await } in
  t

[@@@ocaml.alert "+unstable"]
[@@@else_]

let[@inline] with_suspend ~name:_ ~run:_ f = f ()
let[@inline] prepare_for_await () = { Dla_.release = ignore; await = ignore }

[@@@endif]
