open Types_

type task = unit -> unit

type t = runner = {
  run_async: ls:local_storage -> task -> unit;
  shutdown: wait:bool -> unit -> unit;
  size: unit -> int;
  num_tasks: unit -> int;
}

exception Shutdown

let[@inline] run_async ?(ls = create_local_storage ()) (self : t) f : unit =
  self.run_async ~ls f

let[@inline] shutdown (self : t) : unit = self.shutdown ~wait:true ()

let[@inline] shutdown_without_waiting (self : t) : unit =
  self.shutdown ~wait:false ()

let[@inline] num_tasks (self : t) : int = self.num_tasks ()
let[@inline] size (self : t) : int = self.size ()

let run_wait_block ?ls self (f : unit -> 'a) : 'a =
  let q = Bb_queue.create () in
  run_async ?ls self (fun () ->
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
    ~run_async:(fun ~ls:_ _ ->
      failwith "Runner.dummy: cannot actually run tasks")
    ()

let get_current_runner = get_current_runner
let get_current_storage = get_current_storage
