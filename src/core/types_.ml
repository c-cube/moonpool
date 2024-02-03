module TLS = Thread_local_storage_

type ls_value = ..

(** Key for task local storage *)
module type LS_KEY = sig
  type t
  type ls_value += V of t

  val offset : int
  (** Unique offset *)

  val init : unit -> t
end

type 'a ls_key = (module LS_KEY with type t = 'a)
(** A LS key (task local storage) *)

type task_ls = ls_value array

(** Store the current LS values for the current thread.

    A worker thread is going to cycle through many tasks, each of which
    has its own storage.  This key allows tasks running on the worker
    to access their own storage *)
let k_ls_values : task_ls ref option TLS.key = TLS.new_key (fun () -> None)
