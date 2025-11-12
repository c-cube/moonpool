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

type thread_local_state = {
  mutable runner: runner;
  mutable cur_fiber: fiber;
}

let k_cur_st : thread_local_state TLS.t = TLS.create ()

let _dummy_computation : Picos.Computation.packed =
  let c = Picos.Computation.create () in
  Picos.Computation.cancel c (Failure "dummy fiber") (Printexc.get_callstack 0);
  Picos.Computation.Packed c

let _dummy_fiber = Picos.Fiber.create_packed ~forbid:true _dummy_computation

let[@inline] get_current_runner () : _ option =
  match TLS.get_exn k_cur_st with
  | st -> Some st.runner
  | exception TLS.Not_set -> None

let[@inline] get_current_fiber () : fiber option =
  match TLS.get_exn k_cur_st with
  | { cur_fiber = f; _ } when f != _dummy_fiber -> Some f
  | _ -> None
  | exception TLS.Not_set -> None

let error_get_current_fiber_ =
  "Moonpool: get_current_fiber was called outside of a fiber."

let[@inline] get_current_fiber_exn () : fiber =
  match TLS.get_exn k_cur_st with
  | { cur_fiber = f; _ } when f != _dummy_fiber -> f
  | _ -> failwith error_get_current_fiber_
  | exception TLS.Not_set -> failwith error_get_current_fiber_
