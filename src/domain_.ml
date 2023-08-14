[@@@ifge 5.0]
[@@@ocaml.alert "-unstable"]

let recommended_number () = Domain.recommended_domain_count ()

type t = unit Domain.t

let get_id (self : t) : int = (Domain.get_id self :> int)
let spawn : _ -> t = Domain.spawn
let relax = Domain.cpu_relax
let join = Domain.join

[@@@ocaml.alert "+unstable"]
[@@@else_]

let recommended_number () = 1

type t = Thread.t

let get_id (self : t) : int = Thread.id self
let spawn f : t = Thread.create f ()
let relax () = Thread.yield ()
let join = Thread.join

[@@@endif]
