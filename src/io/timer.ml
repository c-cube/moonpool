open Common_
module Time = Time_

type instant_s = float
type duration_s = float

type kind =
  | Once
  | Every of duration_s

type task = {
  mutable deadline: instant_s;
  mutable active: bool;
  f: cancel_handle -> unit;
  as_cancel_handle: cancel_handle;
  kind: kind;
}

module Task_heap = Heap_.Make (struct
  type t = task

  let[@inline] leq t1 t2 = t1.deadline <= t2.deadline
end)

type t = { mutable tasks: Task_heap.t }

(** accepted time diff for actions. *)
let epsilon_s = 0.000_001

type tick_res =
  | Wait of float
  | Run of (cancel_handle -> unit) * cancel_handle
  | Empty

let[@inline] has_tasks self = not (Task_heap.is_empty self.tasks)
let[@inline] num_tasks self : int = Task_heap.size self.tasks

let[@inline] pop_task_ self : unit =
  let tasks, _t = Task_heap.take_exn self.tasks in
  self.tasks <- tasks

let run_after self delay f : cancel_handle =
  let now = Time.time_s () in
  let deadline = now +. delay in
  let rec task =
    {
      deadline;
      f;
      kind = Once;
      active = true;
      as_cancel_handle = { cancel = (fun () -> task.active <- false) };
    }
  in
  self.tasks <- Task_heap.insert task self.tasks;
  task.as_cancel_handle

let run_every self delay f : cancel_handle =
  let now = Time.time_s () in
  let deadline = now +. delay in
  let rec task =
    {
      deadline;
      f;
      kind = Every delay;
      active = true;
      as_cancel_handle = { cancel = (fun () -> task.active <- false) };
    }
  in
  self.tasks <- Task_heap.insert task self.tasks;
  task.as_cancel_handle

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
        self.tasks <- Task_heap.insert task self.tasks);

      Run (task.f, task.as_cancel_handle)
    ) else
      Wait remaining_time_s

let create () = { tasks = Task_heap.empty }
