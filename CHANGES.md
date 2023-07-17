
# 0.3

- add `Fork_join` for parallelizing computations. This is only
    available on OCaml 5.x because it relies on effects.
- add `Fork_join.{for_,map_array,map_list}`
- add `Fork_join.all_{list,init}`
- add `Pool.with_`
- add a channel module
- add `Runner`, change `Pool` to produce a `Runner.t`
- add a `Lock` module
- add support for domain-local-await when installed
- add `Fut.await` for OCaml >= 5.0

- fix: Fork_join.both_ignore now has a more general type

- expose `Suspend_` and its internal effect with an unstability alert.
    This is intended for implementors of `Runner` only.
- port `cpp.ml` from containers, replace previous codegen with it.
    This will provide better flexibility for supporting multiple versions
    of OCaml in the future.
- add `Pool.run_wait_block`; rename `Pool.run` into `Pool.run_async`
- fix: in blocking queue, `pop` works on a non empty closed queue

# 0.2

- add `Fut.for_list`
- add `around_task` to `Pool.create`
- add `Pool.shutdown_without_waiting`
- add `Pool.num_tasks`
- add `Fut.is_done`
- add `Blocking_queue.size`
- add `Fut.for_array` to easily iterate on an array in parallel
- add `Fut.get_or_fail{,_exn}`

- perf: limit number of work queues in pool
- perf: use multiple queues and non-blocking work-stealing from them, in pool
    this improves the behavior for many small tasks by reducing contention on
    each queue

- fix: fut: actually run all map/bind callbacks in pool if provided

# 0.1.1

- fix(fut): fix bug when calling `wait_list []`
- fix: join_array on arrays of length=1 had a bound error

# 0.1

initial release
