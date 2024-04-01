# Moonpool

[![build](https://github.com/c-cube/moonpool/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/moonpool/actions/workflows/main.yml)

A pool within a bigger pool (ie the ocean). Here, we're talking about
pools of `Thread.t` which live within a fixed pool of `Domain.t`.

This fixed pool of domains is shared between *all* the pools in moonpool.
The rationale is that we should not have more domains than cores, so
it's easier to pre-allocate exactly that many domains, and run more flexible
thread pools on top.

In addition, some concurrency and parallelism primitives are provided:
- `Moonpool.Fut` provides futures/promises that execute
    on these thread pools. The futures are thread safe.
- `Moonpool.Chan` provides simple cooperative and thread-safe channels
    to use within pool-bound tasks. They're essentially re-usable futures.

    On OCaml 5 (meaning there's actual domains and effects, not just threads),
    a `Fut.await` primitive is provided. It's simpler and more powerful
    than the monadic combinators.
- `Moonpool_forkjoin`, in the library `moonpool.forkjoin`
    provides the fork-join parallelism primitives
    to use within tasks running in the pool.

## Usage

The user can create several thread pools (implementing the interface `Runner.t`).
These pools use regular posix threads, but the threads are spread across
multiple domains (on OCaml 5), which enables parallelism.

Current we provide these pool implementations:
- `Fifo_pool` is a thread pool that uses a blocking queue to schedule tasks,
    which means they're picked in the same order they've been scheduled ("fifo").
    This pool is simple and will behave fine for coarse-granularity concurrency,
    but will slow down under heavy contention.
- `Ws_pool` is a work-stealing pool, where each thread has its own local queue
    in addition to a global queue of tasks. This is efficient for workloads
    with many short tasks that spawn other tasks, but the order in which
    tasks are run is less predictable. This is useful when throughput is
    the important thing to optimize.

The function `Runner.run_async pool task` schedules `task()` to run on one of
the workers of `pool`, as soon as one is available. No result is returned by `run_async`.

```ocaml
# #require "threads";;
# let pool = Moonpool.Fifo_pool.create ~num_threads:4 ();;
val pool : Moonpool.Runner.t = <abstr>

# begin
   Moonpool.Runner.run_async pool
    (fun () ->
        Thread.delay 0.1;
        print_endline "running from the pool");
   print_endline "running from the caller";
   Thread.delay 0.3; (* wait for task to run before returning *)
  end ;;
running from the caller
running from the pool
- : unit = ()
```

To wait until the task is done, you can use `Runner.run_wait_block`[^1] instead:

[^1]: beware of deadlock! See documentation for more details.

```ocaml
# begin
   Moonpool.Runner.run_wait_block pool
    (fun () ->
        Thread.delay 0.1;
        print_endline "running from the pool");
   print_endline "running from the caller (after waiting)";
  end ;;
running from the pool
running from the caller (after waiting)
- : unit = ()
```

The function `Fut.spawn ~on f` schedules `f ()` on the pool `on`, and immediately
returns a _future_ which will eventually hold the result (or an exception).

The function `Fut.peek` will return the current value, or `None` if the future is
still not completed.
The functions `Fut.wait_block` and `Fut.wait_block_exn` will
block the current thread and wait for the future to complete.
There are some deadlock risks associated with careless use of these, so
be sure to consult the documentation of the `Fut` module.

```ocaml
# let fut = Moonpool.Fut.spawn ~on:pool
    (fun () ->
       Thread.delay 0.5;
       1+1);;
val fut : int Moonpool.Fut.t = <abstr>

# Moonpool.Fut.peek fut;
- : int Moonpool.Fut.or_error option = None

# Moonpool.Fut.wait_block_exn fut;;
- : int = 2
```

Some combinators on futures are also provided, e.g. to wait for all futures in
an array to complete:

```ocaml
# let rec fib x =
    if x <= 1 then 1 else fib (x-1) + fib (x-2);;
val fib : int -> int = <fun>

# List.init 10 fib;;
- : int list = [1; 1; 2; 3; 5; 8; 13; 21; 34; 55]

# let fibs = Array.init 35 (fun n -> Moonpool.Fut.spawn ~on:pool (fun () -> fib n));;
val fibs : int Moonpool.Fut.t array =
  [|<abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>; <abstr>;
    <abstr>; <abstr>; <abstr>|]

# Moonpool.Fut.join_array fibs |> Moonpool.Fut.wait_block;;
- : int array Moonpool.Fut.or_error =
Ok
 [|1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597; 2584;
   4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
   514229; 832040; 1346269; 2178309; 3524578; 5702887; 9227465|]
```

### Support for `await`

On OCaml 5, effect handlers can be used to implement `Fut.await : 'a Fut.t -> 'a`.

The expression `Fut.await some_fut`, when run from inside some thread pool,
suspends its caller task; the suspended task is then parked, and will
be resumed when the future is completed.
The pool worker that was executing this expression, in the mean time, moves
on to another task.
This means that `await` is free of the deadlock risks associated with
`Fut.wait_block`.

In the following example, we bypass the need for `Fut.join_array` by simply
using regular array functions along with `Fut.await`.

```ocaml
# let main_fut =
    let open Moonpool.Fut in
    spawn ~on:pool @@ fun () ->
    (* array of sub-futures *)
    let tasks: _ Moonpool.Fut.t array = Array.init 100 (fun i ->
       spawn ~on:pool (fun () ->
           Thread.delay 0.01;
           i+1))
    in
    Array.fold_left (fun n fut -> n + await fut) 0 tasks
  ;;
val main_fut : int Moonpool.Fut.t = <abstr>

# let expected_sum = Array.init 100 (fun i->i+1) |> Array.fold_left (+) 0;;
val expected_sum : int = 5050

# assert (expected_sum = Moonpool.Fut.wait_block_exn main_fut);;
- : unit = ()
```

### Errors

We have a `Exn_bt.t` type that comes in handy in many places. It bundles together
an exception and the backtrace associated with the place the exception was caught.

### Fibers

On OCaml 5, Moonpool comes with a library `moonpool.fib` (module `Moonpool_fib`)
which provides _lightweight fibers_
that can run on any Moonpool runner.
These fibers are a sort of lightweight thread, dispatched on the runner's
background thread(s).
Fibers rely on effects to implement `Fiber.await`, suspending themselves until the `await`-ed fiber
is done.

```ocaml
# #require "moonpool.fib";;
...

# (* convenient alias *)
  module F = Moonpool_fib;;
module F = Moonpool_fib
# F.main (fun _runner ->
    let f1 = F.spawn (fun () -> fib 10) in
    let f2 = F.spawn (fun () -> fib 15) in
    F.await f1 + F.await f2);;
- : int = 1076
```

Fibers form a _tree_, where a fiber calling `Fiber.spawn` to start a sub-fiber is
the sub-fiber's _parent_.
When a parent fails, all its children are cancelled (forced to fail).
This is a simple form of [Structured Concurrency](https://en.wikipedia.org/wiki/Structured_concurrency).

Like a future, a fiber eventually _resolves_ into a value (or an `Exn_bt.t`) that it's possible
to `await`. With `Fiber.res : 'a Fiber.t -> 'a Fut.t` it's possible to access that result
as a regular future, too.
However, this resolution is only done after all the children of the fiber have
resolved — the lifetime of fibers forms a well-nested tree in that sense.

When a fiber is suspended because it `await`s another fiber (or future), the scheduler's
thread on which it was running becomes available again and can go on process another task.
When the fiber resumes, it will automatically be re-scheduled on the same runner it started on.
This means fibers on pool P1 can await fibers from pool P2 and still be resumed on P1.

In addition to all that, fibers provide _fiber local storage_ (like thread-local storage, but per fiber).
This storage is inherited in `spawn` (as a shallow copy only — it's advisable to only
put persistent data in storage to avoid confusing aliasing).
The storage is convenient for carrying around context for cross-cutting concerns such
as logging or tracing (e.g. a log tag for the current user or request ID, or a tracing
scope).

### Fork-join

On OCaml 5, again using effect handlers, the sublibrary `moonpool.forkjoin`
provides a module `Moonpool_forkjoin`
implements the [fork-join model](https://en.wikipedia.org/wiki/Fork%E2%80%93join_model).
It must run on a pool (using `Runner.run_async` or inside a future via `Fut.spawn`).

It is generally better to use the work-stealing pool for workloads that rely on
fork-join for better performance, because fork-join will tend to spawn lots of
shorter tasks.

Here is an simple example of a parallel sort.
It uses selection sort for small slices, like this:

```ocaml
# let rec select_sort arr i len =
    if len >= 2 then ( 
      let idx = ref i in
      for j = i+1 to i+len-1 do
        if arr.(j) < arr.(!idx) then idx := j
      done;
      let tmp = arr.(!idx) in
      arr.(!idx) <- arr.(i);
      arr.(i) <- tmp;
      select_sort arr (i+1) (len-1)
    );;
val select_sort : 'a array -> int -> int -> unit = <fun>
```

And a parallel quicksort for larger slices:

```ocaml
# let rec quicksort arr i len : unit =
    if len <= 10 then select_sort arr i len
    else (
      let pivot = arr.(i + (len / 2)) in
      let low = ref (i - 1) in
      let high = ref (i + len) in

      (* partition the array slice *)
      while !low < !high do
        incr low;
        decr high;
        while arr.(!low) < pivot do
          incr low
        done;
        while arr.(!high) > pivot do
          decr high
        done;
        if !low < !high then (
          let tmp = arr.(!low) in
          arr.(!low) <- arr.(!high);
          arr.(!high) <- tmp
        )
      done;

      (* sort lower half and upper half in parallel *)
      Moonpool_forkjoin.both_ignore
        (fun () -> quicksort arr i (!low - i))
        (fun () -> quicksort arr !low (len - (!low - i)))
    );;
val quicksort : 'a array -> int -> int -> unit = <fun>


# let arr = [| 4;2;1;5;1;10;3 |];;
val arr : int array = [|4; 2; 1; 5; 1; 10; 3|]
# Moonpool.Fut.spawn
    ~on:pool (fun () -> quicksort arr 0 (Array.length arr))
    |> Moonpool.Fut.wait_block_exn;;
- : unit = ()
# arr;;
- : int array = [|1; 1; 2; 3; 4; 5; 10|]


# let arr =
    let rand = Random.State.make [| 42 |] in
    Array.init 40 (fun _-> Random.State.int rand 300);;
val arr : int array =
  [|64; 220; 247; 196; 51; 186; 22; 106; 58; 58; 11; 161; 243; 111; 74; 109;
    49; 135; 59; 192; 132; 38; 19; 44; 126; 147; 182; 83; 95; 231; 204; 121;
    142; 255; 72; 85; 95; 93; 73; 202|]
# Moonpool.Fut.spawn ~on:pool
    (fun () -> quicksort arr 0 (Array.length arr))
  |> Moonpool.Fut.wait_block_exn
  ;;
- : unit = ()
# arr;;
- : int array =
[|11; 19; 22; 38; 44; 49; 51; 58; 58; 59; 64; 72; 73; 74; 83; 85; 93; 95; 95;
  106; 109; 111; 121; 126; 132; 135; 142; 147; 161; 182; 186; 192; 196; 202;
  204; 220; 231; 243; 247; 255|]
```

Note that the sort had to be started in a task (via `Moonpool.Fut.spawn`)
so that fork-join would run on the thread pool.
This is necessary even for the initial iteration because fork-join
relies on OCaml 5's effects, meaning that the computation needs to run
inside an effect handler provided by the thread pool.

### More intuition

To quote [gasche](https://discuss.ocaml.org/t/ann-moonpool-0-1/12387/15):

<blockquote>
You are assuming that, if pool P1 has 5000 tasks, and pool P2 has 10 other tasks, then these 10 tasks will get to run faster than if we just added them at the end of pool P1. This sounds like a “fairness” assumption: separate pools will get comparable shares of domain compute ressources, or at least no pool will be delayed too much from running their first tasks.

[…]

- each pool uses a fixed number of threads, all running simultaneously; if there are more tasks sent to the pool, they are delayed and will only get one of the pool threads when previous tasks have finished
- separate pools run their separate threads simultaneously, so they compete for compute resources on their domain using OCaml’s systhreads scheduler – which does provide fairness in practice
- as a result, running in a new pool enables quicker completion than adding to an existing pool (as we will be scheduled right away instead of waiting for previous tasks in our pool to free some threads)
- the ratio of compute resources that each pool gets should be roughly proportional to its number of worker threads
</blockquote>

## OCaml versions

This works for OCaml >= 4.08.
- On OCaml 4.xx, there are no domains, so this is just a library for regular thread pools
    with not actual parallelism (except for threads that call C code that releases the runtime lock, that is).
    C calls that do release the runtime lock (e.g. to call [Z3](https://github.com/Z3Prover/z3), hash a file, etc.)
    will still run in parallel.
- on OCaml 5.xx, there is a fixed pool of domains (using the recommended domain count).
    These domains do not do much by themselves, but we schedule new threads on them, and form pools
    of threads that contain threads from each domain.
    Each domain might thus have multiple threads that belong to distinct pools (and several threads from
    the same pool, too — this is useful for threads blocking on IO); Each pool will have threads
    running on distinct domains, which enables parallelism.

    A useful analogy is that each domain is a bit like a CPU core, and `Thread.t` is a logical thread running on a core.
    Multiple threads have to share a single core and do not run in parallel on it[^2].
    We can therefore build pools that spread their worker threads on multiple cores to enable parallelism within each pool.

TODO: actually use https://github.com/haesbaert/ocaml-processor to pin domains to cores,
possibly optionally using `select` in dune.

## License

MIT license.

## Install

```sh, skip
$ opam install moonpool
```

[^2]: ignoring hyperthreading for the sake of the analogy.
