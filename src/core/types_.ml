module TLS = Thread_local_storage_
module Domain_pool_ = Moonpool_dpool

type ls_value = ..

(** Key for task local storage *)
module type LS_KEY = sig
  type t
  type ls_value += V of t

  val offset : int
  (** Unique offset *)

  val init : unit -> t
end

type 'a ls_key = (module LS_KEY with type t = 'a)
(** A LS key (task local storage) *)

type task = unit -> unit
type local_storage = ls_value array ref

type runner = {
  run_async: ls:local_storage -> task -> unit;
  shutdown: wait:bool -> unit -> unit;
  size: unit -> int;
  num_tasks: unit -> int;
}

let k_cur_runner : runner option ref TLS.key = TLS.new_key (fun () -> ref None)

let k_cur_storage : local_storage option ref TLS.key =
  TLS.new_key (fun () -> ref None)

let[@inline] get_current_runner () : _ option = !(TLS.get k_cur_runner)
let[@inline] get_current_storage () : _ option = !(TLS.get k_cur_storage)
let[@inline] create_local_storage () = ref [||]
