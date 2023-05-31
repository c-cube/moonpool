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

let spawn_on f : t =
  Thread.create f ()
|}

let domain_post_5 =
  {|
let recommended_number () = Domain.recommended_domain_count ()

type t = unit Domain.t

let get_id (self:t) : int = (Domain.get_id self :> int)

let spawn f : t =
  Domain.spawn f ()
|}

let p_version s = Scanf.sscanf s "%d.%d" (fun x y -> x, y)

let () =
  let atomic = ref false in
  let domain = ref false in
  let ocaml = ref Sys.ocaml_version in
  Arg.parse
    [
      "--atomic", Arg.Set atomic, " atomic";
      "--domain", Arg.Set domain, " domain";
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
  )
