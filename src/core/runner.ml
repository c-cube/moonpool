open Types_

type fiber = Picos.Fiber.t
type task = unit -> unit

type t = runner = {
  run_async: fiber:fiber -> task -> unit;
  shutdown: wait:bool -> unit -> unit;
  size: unit -> int;
  num_tasks: unit -> int;
}

exception Shutdown

let[@inline] run_async ?fiber (self : t) f : unit =
  let fiber =
    match fiber with
    | Some f -> f
    | None ->
      let comp = Picos.Computation.create () in
      Picos.Fiber.create ~forbid:false comp
  in
  self.run_async ~fiber f

let[@inline] shutdown (self : t) : unit = self.shutdown ~wait:true ()

let[@inline] shutdown_without_waiting (self : t) : unit =
  self.shutdown ~wait:false ()

let[@inline] num_tasks (self : t) : int = self.num_tasks ()
let[@inline] size (self : t) : int = self.size ()

let run_wait_block ?fiber self (f : unit -> 'a) : 'a =
  let q = Bb_queue.create () in
  run_async ?fiber self (fun () ->
      try
        let x = f () in
        Bb_queue.push q (Ok x)
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Bb_queue.push q (Error (exn, bt)));
  match Bb_queue.pop q with
  | Ok x -> x
  | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

module For_runner_implementors = struct
  let create ~size ~num_tasks ~shutdown ~run_async () : t =
    { size; num_tasks; shutdown; run_async }

  let k_cur_runner : t TLS.t = Types_.k_cur_runner
end

let dummy : t =
  For_runner_implementors.create
    ~size:(fun () -> 0)
    ~num_tasks:(fun () -> 0)
    ~shutdown:(fun ~wait:_ () -> ())
    ~run_async:(fun ~fiber:_ _ ->
      failwith "Runner.dummy: cannot actually run tasks")
    ()

let get_current_runner = get_current_runner
let get_current_fiber = get_current_fiber
