module Time = Time_

type instant_s = float
type duration_s = float

type tick_res =
  | Wait of float
  | Run of (unit -> unit)
  | Empty

type kind =
  | Once
  | Every of duration_s

type task = {
  handle: Handle.t;
  mutable deadline: instant_s;
  mutable active: bool;
  mutable f: unit -> unit;
  kind: kind;
}

module Task_heap = Heap_.Make (struct
  type t = task

  let[@inline] leq t1 t2 = t1.deadline <= t2.deadline
end)

type t = {
  mutable tasks: Task_heap.t;
  by_handle: task Handle.Tbl.t;
}

(** accepted time diff for actions. *)
let epsilon_s = 0.000_001

let[@inline] has_tasks self = not (Task_heap.is_empty self.tasks)

let pop_task_ self : unit =
  let tasks, t = Task_heap.take_exn self.tasks in
  Handle.Tbl.remove self.by_handle t.handle;
  self.tasks <- tasks

let cancel (self : t) (h : Handle.t) =
  match Handle.Tbl.find_opt self.by_handle h with
  | None -> ()
  | Some t ->
    t.active <- false;
    t.f <- ignore (* free memory captured by [f] *)

let run_after_s self delay handle f : unit =
  let now = Time.time_s () in
  let deadline = now +. delay in
  let task = { handle; deadline; f; kind = Once; active = true } in
  self.tasks <- Task_heap.insert task self.tasks;
  Handle.Tbl.add self.by_handle handle task

let run_every_s self delay handle f : unit =
  let now = Time.time_s () in
  let deadline = now +. delay in
  let task = { handle; deadline; f; kind = Every delay; active = true } in
  self.tasks <- Task_heap.insert task self.tasks;
  Handle.Tbl.add self.by_handle handle task

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
        (* schedule the next iteration. If we're past the deadline,
           use the deadline as the starting point for the next iteration. *)
        task.deadline <- min now task.deadline +. dur;
        self.tasks <- Task_heap.insert task self.tasks);

      Run task.f
    ) else
      Wait remaining_time_s

let create () = { tasks = Task_heap.empty; by_handle = Handle.Tbl.create 32 }
