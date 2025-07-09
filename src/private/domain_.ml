[@@@ocaml.alert "-unstable"]

let recommended_number () = Domain.recommended_domain_count ()

type t = unit Domain.t

let get_id (self : t) : int = (Domain.get_id self :> int)
let spawn : _ -> t = Domain.spawn
let relax = Domain.cpu_relax
let join = Domain.join
let is_main_domain = Domain.is_main_domain
