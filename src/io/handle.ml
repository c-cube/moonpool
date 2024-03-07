open Common_

type handle_type =
  | H_read
  | H_write
  | H_timer

type t = int

let n_bits_shard = 3
let n_shards_ = 8
let shard_mask_ = (1 lsl n_bits_shard) - 1
let () = assert (1 lsl n_bits_shard = n_shards_)

(* array of counters, sharded on thread ID to reduce contention.
   TODO: avoid false sharing *)
let generators_ : int A.t array = Array.init n_shards_ (fun _ -> A.make 0)

let int_of_handle_type = function
  | H_read -> 0
  | H_write -> 1
  | H_timer -> 2

let[@inline] handle_type (self : t) : handle_type =
  match self land 0b11 with
  | 0 -> H_read
  | 1 -> H_write
  | 2 -> H_timer
  | _ -> assert false

let fresh (ty : handle_type) : t =
  let shard = (Thread.id @@ Thread.self ()) land shard_mask_ in
  let n = A.fetch_and_add (Array.unsafe_get generators_ shard) 1 in
  let n =
    (n lsl (2 + n_bits_shard)) lor (shard lsl 2) lor int_of_handle_type ty
  in
  n

module As_key = struct
  type t = int

  let equal : t -> t -> bool = ( = )
  let hash = Hashtbl.hash
  let compare : t -> t -> int = Stdlib.compare
end

module Map = Map.Make (As_key)
module Tbl = Hashtbl.Make (As_key)
