module A = Atomic

type t = int

let counter_ = A.make 0
let equal : t -> t -> bool = ( = )
let compare : t -> t -> int = Stdlib.compare
let[@inline] generate_fresh () = A.fetch_and_add counter_ 1

(* TODO: better hash *)
let[@inline] hash x = x land max_int

module Set = Set.Make (Int)
module Map = Map.Make (Int)
