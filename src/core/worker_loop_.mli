(** Internal module that is used for workers.

    A thread pool should use this [worker_loop] to run tasks,
    handle effects, etc. *)

open Types_

type task_full =
  | T_start of {
      fiber: fiber;
      f: unit -> unit;
    }
  | T_resume : {
      fiber: fiber;
      k: unit -> unit;
    }
      -> task_full

val _dummy_task : task_full

type around_task =
  | AT_pair : (Runner.t -> 'a) * (Runner.t -> 'a -> unit) -> around_task

exception No_more_tasks

type 'st ops = {
  schedule: 'st -> task_full -> unit;
  get_next_task: 'st -> task_full;
  get_thread_state: unit -> 'st;
  around_task: 'st -> around_task;
  on_exn: 'st -> Exn_bt.t -> unit;
  runner: 'st -> Runner.t;
  before_start: 'st -> unit;
  cleanup: 'st -> unit;
}

val worker_loop : block_signals:bool -> ops:'st ops -> 'st -> unit
