module A = Atomic_

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

(* DLA interop *)
let prepare_for_await () : Dla_.t =
  (* current state *)
  let st : ((with_handler:bool -> task -> unit) * suspension) option A.t =
    A.make None
  in

  let release () : unit =
    match A.exchange st None with
    | None -> ()
    | Some (run, k) -> run ~with_handler:true (fun () -> k (Ok ()))
  and await () : unit =
    suspend { handle = (fun ~run k -> A.set st (Some (run, k))) }
  in

  let t = { Dla_.release; await } in
  t

[@@@ocaml.alert "+unstable"]
[@@@else_]

let with_suspend ~run:_ f = f ()
let prepare_for_await () = { Dla_.release = ignore; await = ignore }

[@@@endif]
