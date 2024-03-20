
# 0.6

- breaking: remove `Immediate_runner` (bug prone and didn't
    handle effects). `Moonpool_fib.main` can be used to handle
    effects in the main function.
- remove deprecated alias `Moonpool.Pool`

- feat: add structured concurrency sub-library `moonpool.fib` with
    fibers. Fibers can use `await` and spawn other fibers that will
    be appropriately cancelled when their parent is.
- feat: add add `moonpool-lwt` as an experimental bridge between moonpool and lwt.
    This allows moonpool runners to be used from within Lwt to
    perform background computations, and conversely to call Lwt from
    moonpool with some precautions.
- feat: task-local storage in the main moonpool runners, available from
    fibers and regular tasks.
- feat: add `Exn_bt` to core
- feat: add `Runner.dummy`
- make `moonpool.forkjoin` optional (only on OCaml >= 5.0)
- feat: add `Fut.Advanced.barrier_on_abstract_container_of_futures`
- feat: add `Fut.map_list`

- refactor: split off domain pool to `moonpool.dpool`
- fix too early exit in Ws_pool

# 0.5.1

- fix `Ws_pool`: workers would exit before processing
    all remaining tasks upon shutdown

# 0.5

## features

- add `Bb_queue.transfer`
- add `Bb_queue.to_{iter,gen,seq}`
- add `Fifo_pool`, a simple pool with a single blocking queue for
    workloads with coarse granularity tasks that value
    latency (e.g. a web server)
- add a work-stealing pool for heavy compute workloads that
    feature a lot of await/fork-join, with a lot of help
    from Vesa Karvonen (@polytypic)
- add `Fut.spawn_on_current_runner`
- add `Runner.{spawn_on_current_runner, await}`
- add a few more toplevel aliases in `Moonpool` itself
- add `No_runner`: a runner that runs tasks synchronously in the caller
- on shutdown, pools will finish running all present tasks before
    closing. New tasks are immediately rejected.

- use an optional dependency on `thread-local-storage` to
    implement work stealing and `spawn_on_current_runner`

## optimizations

- use the main domain to spawn threads on it. This means we can really
    use all cores, not all but one.
- in `Fork_join.both`, only one of the two sides schedules a task,
    the other runs in the current thread. This reduces scheduling overhead.
- compare to domainslib in benchmarks. With the WS pool we're now slightly
    ahead in terms of overhead on the recursive fib benchmark.

## breaking

- deprecate `Pool`, now an alias to `Fifo_pool`
- the `Fut.Infix_local` and `Fut.infix` are gone, replaced with
    a simpler `Fut.Infix` module that tries to use the current runner
    for intermediate tasks.

# 0.4

- add `Fut.{reify_error,bind_reify_error}`
- full lifecycle for worker domains, where a domain
    will shutdown if no thread runs on it, after a
    short delay.

- fix: generalize type of `create_arg`
- perf: in `Bb_queue`, only signal condition on push if queue was empty

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
