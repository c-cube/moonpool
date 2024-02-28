(** Task-local storage.

    This storage is associated to the current task,
    just like thread-local storage is associated with
    the current thread. The storage is carried along in case
    the current task is suspended.

    @since NEXT_RELEASE
*)

type storage
(** Underlying storage for a task *)

type 'a key
(** A key used to access a particular (typed) storage slot on every task. *)

val new_key : init:(unit -> 'a) -> unit -> 'a key
(** [new_key ~init ()] makes a new key. Keys are expensive and
    should never be allocated dynamically or in a loop.
    The correct pattern is, at toplevel:

    {[
      let k_foo : foo Task_ocal_storage.key =
        Task_local_storage.new_key ~init:(fun () -> make_foo ()) ()

    (* … *)

    (* use it: *)
    let … = Task_local_storage.get k_foo
    ]}
*)

val get : 'a key -> 'a
(** [get k] gets the value for the current task for key [k].
    Must be run from inside a task running on a runner.
    @raise Failure otherwise *)

val set : 'a key -> 'a -> unit
(** [set k v] sets the storage for [k] to [v].
    Must be run from inside a task running on a runner.
    @raise Failure otherwise *)

val with_value : 'a key -> 'a -> (unit -> 'b) -> 'b
(** [with_value k v f] sets [k] to [v] for the duration of the call
    to [f()]. When [f()] returns (or fails), [k] is restored
    to its old value. *)

(**/**)

module Private_ : sig
  module Storage : sig
    type t = storage

    val k_storage : t ref option Thread_local_storage_.key
    val create : unit -> t
    val copy : t -> t
    val copy_of_current : unit -> t
    val dummy : t
  end
end

(**/**)
