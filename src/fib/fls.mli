(** Fiber-local storage.

    This storage is associated to the current fiber,
    just like thread-local storage is associated with
    the current thread.

    See {!Moonpool.Task_local_storage} for more general information, as
    this is based on it.

    {b NOTE}: it's important to note that, while each fiber
    has its own storage, spawning a sub-fiber [f2] from a fiber [f1]
    will only do a shallow copy of the storage.
    Values inside [f1]'s storage will be physically shared with [f2].
*)

include module type of struct
  include Task_local_storage
end
