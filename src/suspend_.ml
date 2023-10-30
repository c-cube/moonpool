type suspension = (unit, exn * Printexc.raw_backtrace) result -> unit
type task = unit -> unit
type 'a iter = ('a -> unit) -> unit

type suspension_handler = {
  handle:
    run:(task -> unit) -> run_batch:(task iter -> unit) -> suspension -> unit;
}
[@@unboxed]

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

module A = Atomic_

type _ Effect.t += Suspend : suspension_handler -> unit Effect.t

let[@inline] suspend h = Effect.perform (Suspend h)

let with_suspend ~(run : task -> unit) ~run_batch (f : unit -> unit) : unit =
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
          h.handle ~run ~run_batch k')
    | _ -> None
  in

  E.try_with f () { E.effc }

(* DLA interop *)
let prepare_for_await () : Dla_.t =
  (* current state *)
  let st : ((task -> unit) * suspension) option A.t = A.make None in

  let release () : unit =
    match A.exchange st None with
    | None -> ()
    | Some (run, k) -> run (fun () -> k (Ok ()))
  and await () : unit =
    suspend { handle = (fun ~run ~run_batch:_ k -> A.set st (Some (run, k))) }
  in

  let t = { Dla_.release; await } in
  t

[@@@ocaml.alert "+unstable"]
[@@@else_]

let[@inline] with_suspend ~run:_ ~run_batch:_ f = f ()
let[@inline] prepare_for_await () = { Dla_.release = ignore; await = ignore }

[@@@endif]
