(** Task-local storage.

    This storage is associated to the current task, just like thread-local
    storage is associated with the current thread. The storage is carried along
    in case the current task is suspended.

    @since 0.6 *)

type 'a t = 'a Picos.Fiber.FLS.t

val create : unit -> 'a t
(** [create ()] makes a new key. Keys are expensive and should never be
    allocated dynamically or in a loop. *)

exception Not_set

val get_exn : 'a t -> 'a
(** [get k] gets the value for the current task for key [k]. Must be run from
    inside a task running on a runner.
    @raise Not_set otherwise *)

val get_opt : 'a t -> 'a option
(** [get_opt k] gets the current task's value for key [k], or [None] if not run
    from inside the task. *)

val get : 'a t -> default:'a -> 'a

val set : 'a t -> 'a -> unit
(** [set k v] sets the storage for [k] to [v]. Must be run from inside a task
    running on a runner.
    @raise Failure otherwise *)

val with_value : 'a t -> 'a -> (unit -> 'b) -> 'b
(** [with_value k v f] sets [k] to [v] for the duration of the call to [f()].
    When [f()] returns (or fails), [k] is restored to its old value. *)

(** {2 Local [Hmap.t]}

    This requires [hmap] to be installed. *)

include module type of struct
  include Hmap_ls_
end
