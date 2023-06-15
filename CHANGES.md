
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
