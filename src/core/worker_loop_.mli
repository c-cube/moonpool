(** Internal module that is used for workers.

    A thread pool should use this [worker_loop] to run tasks, handle effects,
    etc. *)

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

exception No_more_tasks

type 'st ops = {
  schedule: 'st -> task_full -> unit;
  get_next_task: 'st -> task_full;
  on_exn: 'st -> Exn_bt.t -> unit;
  runner: 'st -> Runner.t;
  before_start: 'st -> unit;
  cleanup: 'st -> unit;
}

module type FINE_GRAINED_ARGS = sig
  type st

  val ops : st ops
  val st : st
end

module Fine_grained (_ : FINE_GRAINED_ARGS) () : sig
  val setup : block_signals:bool -> unit -> unit
  (** Just initialize the loop *)

  val run : ?max_tasks:int -> unit -> unit
  (** Run the loop until no task remains or until [max_tasks] tasks have been
      run *)

  val teardown : unit -> unit
  (** Tear down the loop *)
end

val worker_loop : block_signals:bool -> ops:'st ops -> 'st -> unit
