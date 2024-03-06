type instant_s = float
type duration_s = float

type kind =
  | Once
  | Every of duration_s

type task = {
  mutable deadline: instant_s;
  mutable active: bool;
  f: Cancel_handle.t -> unit;
  as_cancel_handle: Cancel_handle.t;
  kind: kind;
}

module Task_heap = Heap.Make (struct
  type t = task

  let[@inline] leq t1 t2 = t1.deadline <= t2.deadline
end)

type t = {
  mutable tasks: Task_heap.t;
  mutable n_tasks: int;
}

(** accepted time diff for actions.*)
let epsilon_s = 0.000_001

type tick_res =
  | Wait of float
  | Run of (Cancel_handle.t -> unit) * Cancel_handle.t
  | Empty

let[@inline] has_tasks self = not (Task_heap.is_empty self.tasks)

let[@inline] pop_task_ self : unit =
  let tasks, _t = Task_heap.take_exn self.tasks in
  self.n_tasks <- self.n_tasks - 1;
  self.tasks <- tasks

let run_after_s self delay cancel f : unit =
  let now = Time.time_s () in
  let deadline = now +. delay in
  let task =
    { deadline; f; kind = Once; active = true; as_cancel_handle = cancel }
  in
  self.tasks <- Task_heap.insert task self.tasks;
  self.n_tasks <- 1 + self.n_tasks;
  Cancel_handle.on_cancel cancel (fun () -> task.active <- false)

let run_every_s self delay cancel f : unit =
  let now = Time.time_s () in
  let deadline = now +. delay in
  let task =
    {
      deadline;
      f;
      kind = Every delay;
      active = true;
      as_cancel_handle = cancel;
    }
  in
  self.tasks <- Task_heap.insert task self.tasks;
  self.n_tasks <- 1 + self.n_tasks;
  Cancel_handle.on_cancel cancel (fun () -> task.active <- false)

let rec next (self : t) : tick_res =
  match Task_heap.find_min self.tasks with
  | None -> Empty
  | Some task when not task.active ->
    pop_task_ self;
    next self
  | Some task ->
    let now = Time.time_s () in

    let remaining_time_s = task.deadline -. now in
    if remaining_time_s <= epsilon_s then (
      pop_task_ self;

      (match task.kind with
      | Once -> ()
      | Every dur ->
        (* schedule the next iteration *)
        task.deadline <- now +. dur;
        self.n_tasks <- 1 + self.n_tasks;
        self.tasks <- Task_heap.insert task self.tasks);

      Run (task.f, task.as_cancel_handle)
    ) else
      Wait remaining_time_s

let create () = { tasks = Task_heap.empty; n_tasks = 0 }
