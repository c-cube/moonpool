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
- `Moonpool.Fork_join` provides the fork-join parallelism primitives
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

### Fork-join

On OCaml 5, again using effect handlers, the module `Fork_join`
implements the [fork-join model](https://en.wikipedia.org/wiki/Fork%E2%80%93join_model).
It must run on a pool (using [Runner.run_async] or inside a future via [Fut.spawn]).

It is generally better to use the work-stealing pool for workloads that rely on
fork-join for better performance, because fork-join will tend to spawn lots of
shorter tasks.

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

# let rec quicksort arr i len : unit =
    if len <= 10 then select_sort arr i len
    else (
      let pivot = arr.(i + (len / 2)) in
      let low = ref (i - 1) in
      let high = ref (i + len) in

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

      Moonpool.Fork_join.both_ignore
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
- on OCaml 5.xx, there is a fixed pool of domains (using the recommended domain count).
    These domains do not do much by themselves, but we schedule new threads on them, and group
    threads from each domain into pools.
    Each domain might thus have multiple threads that belong to distinct pools (and several threads from
    the same pool, too — this is useful for threads blocking on IO).

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

[^2]: let's not talk about hyperthreading.
