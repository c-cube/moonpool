(** Fiber-local storage.

    This storage is associated to the current fiber,
    just like thread-local storage is associated with
    the current thread.
*)

include module type of struct
  include Task_local_storage
end
