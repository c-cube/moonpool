let atomic_pre_412 =
  {|
type 'a t = { mutable x: 'a }

let[@inline] make x = { x }
let[@inline] get { x } = x
let[@inline] set r x = r.x <- x

let[@inline never] exchange r x =
  (* atomic *)
  let y = r.x in
  r.x <- x;
  (* atomic *)
  y

let[@inline never] compare_and_set r seen v =
  (* atomic *)
  if r.x == seen then (
    r.x <- v;
    (* atomic *)
    true
  ) else
    false

let[@inline never] fetch_and_add r x =
  (* atomic *)
  let v = r.x in
  r.x <- x + r.x;
  (* atomic *)
  v

let[@inline never] incr r =
  (* atomic *)
  r.x <- 1 + r.x
(* atomic *)

let[@inline never] decr r =
  (* atomic *)
  r.x <- r.x - 1
(* atomic *)

|}

let atomic_post_412 = {|
include Atomic
|}

let domain_pre_5 =
  {|
let recommended_number () = 1

type t = Thread.t

let get_id (self:t) : int = Thread.id self

let spawn f : t =
  Thread.create f ()

let relax () = Thread.yield ()
|}

let domain_post_5 =
  {|
let recommended_number () = Domain.recommended_domain_count ()

type t = unit Domain.t

let get_id (self:t) : int = (Domain.get_id self :> int)

let spawn : _ -> t = Domain.spawn

let relax = Domain.cpu_relax
|}

let suspend_pre_5 =
  {|
let suspend _ = failwith "Thread suspension is only available on OCaml >= 5.0"
let with_suspend ~run:_ f : unit = f()
|}

let suspend_post_5 =
  {|
open Suspend_types_

type _ Effect.t +=
  | Suspend : suspension_handler -> unit Effect.t

let[@inline] suspend h = Effect.perform (Suspend h)

let with_suspend ~(run:with_handler:bool -> task -> unit) (f: unit -> unit) : unit =
  let module E = Effect.Deep in

  (* effect handler *)
  let effc
  : type e. e Effect.t -> ((e, _) E.continuation -> _) option
  = function
    | Suspend h ->
      Some (fun k ->
        let k': suspension = function
          | Ok () -> E.continue k ()
          | Error (exn, bt) ->
            E.discontinue_with_backtrace k exn bt
        in
        h.handle ~run k'
      )
    | _ -> None
  in

  E.try_with f () {E.effc}
|}

let p_version s = Scanf.sscanf s "%d.%d" (fun x y -> x, y)

let () =
  let atomic = ref false in
  let domain = ref false in
  let suspend = ref false in
  let ocaml = ref Sys.ocaml_version in
  Arg.parse
    [
      "--atomic", Arg.Set atomic, " atomic";
      "--domain", Arg.Set domain, " domain";
      "--suspend", Arg.Set suspend, " suspend";
      "--ocaml", Arg.Set_string ocaml, " set ocaml version";
    ]
    ignore "";

  let major, minor = p_version !ocaml in

  if !atomic then (
    let code =
      if (major, minor) < (4, 12) then
        atomic_pre_412
      else
        atomic_post_412
    in
    print_endline code
  ) else if !domain then (
    let code =
      if (major, minor) < (5, 0) then
        domain_pre_5
      else
        domain_post_5
    in
    print_endline code
  ) else if !suspend then (
    let code =
      if (major, minor) < (5, 0) then
        suspend_pre_5
      else
        suspend_post_5
    in
    print_endline code
  )
