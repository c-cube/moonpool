module TLS = Thread_local_storage
module Domain_pool_ = Moonpool_dpool

type task = unit -> unit
type fiber = Picos.Fiber.t

type runner = {
  run_async: fiber:fiber -> task -> unit;
  shutdown: wait:bool -> unit -> unit;
  size: unit -> int;
  num_tasks: unit -> int;
}

let k_cur_runner : runner TLS.t = TLS.create ()
let k_cur_fiber : fiber TLS.t = TLS.create ()

let _dummy_computation : Picos.Computation.packed =
  let c = Picos.Computation.create () in
  Picos.Computation.cancel c
    { exn = Failure "dummy fiber"; bt = Printexc.get_callstack 0 };
  Picos.Computation.Packed c

let _dummy_fiber = Picos.Fiber.create_packed ~forbid:true _dummy_computation
let[@inline] get_current_runner () : _ option = TLS.get_opt k_cur_runner

let[@inline] get_current_fiber () : fiber option =
  match TLS.get_exn k_cur_fiber with
  | f when f != _dummy_fiber -> Some f
  | _ -> None

let[@inline] get_current_fiber_exn () : fiber =
  match TLS.get_exn k_cur_fiber with
  | f when f != _dummy_fiber -> f
  | _ -> failwith "Moonpool: get_current_fiber was called outside of a fiber."
