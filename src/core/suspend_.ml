open Types_
module A = Atomic_

type suspension = unit Exn_bt.result -> unit
type task = unit -> unit

type suspension_handler = {
  handle:
    ls:task_ls ->
    run:(name:string -> task -> unit) ->
    resume:(ls:task_ls -> suspension -> unit Exn_bt.result -> unit) ->
    suspension ->
    unit;
}
[@@unboxed]

[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

type _ Effect.t +=
  | Suspend : suspension_handler -> unit Effect.t
  | Yield : unit Effect.t

let[@inline] yield () = Effect.perform Yield
let[@inline] suspend h = Effect.perform (Suspend h)

let with_suspend ~on_suspend ~(run : name:string -> task -> unit)
    ~(resume : ls:task_ls -> suspension -> unit Exn_bt.result -> unit)
    (f : unit -> unit) : unit =
  let module E = Effect.Deep in
  (* effect handler *)
  let effc : type e. e Effect.t -> ((e, _) E.continuation -> _) option =
    function
    | Suspend h ->
      (* TODO: discontinue [k] if current fiber (if any) is cancelled? *)
      Some
        (fun k ->
          let ls = on_suspend () in
          let k' : suspension = function
            | Ok () -> E.continue k ()
            | Error (exn, bt) -> E.discontinue_with_backtrace k exn bt
          in
          h.handle ~ls ~run ~resume k')
    | Yield ->
      (* TODO: discontinue [k] if current fiber (if any) is cancelled? *)
      Some
        (fun k ->
          let ls = on_suspend () in
          let k' : suspension = function
            | Ok () -> E.continue k ()
            | Error (exn, bt) -> E.discontinue_with_backtrace k exn bt
          in
          resume ~ls k' (Ok ()))
    | _ -> None
  in

  E.try_with f () { E.effc }

(* DLA interop *)
let prepare_for_await () : Dla_.t =
  (* current state *)
  let st : (_ * _ * suspension) option A.t = A.make None in

  let release () : unit =
    match A.exchange st None with
    | None -> ()
    | Some (ls, resume, k) -> resume ~ls k @@ Ok ()
  and await () : unit =
    suspend
      { handle = (fun ~ls ~run:_ ~resume k -> A.set st (Some (ls, resume, k))) }
  in

  let t = { Dla_.release; await } in
  t

[@@@ocaml.alert "+unstable"]
[@@@else_]

let[@inline] with_suspend ~on_suspend:_ ~run:_ ~resume:_ f = f ()
let[@inline] prepare_for_await () = { Dla_.release = ignore; await = ignore }

[@@@endif]
