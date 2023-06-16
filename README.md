# Moonpool

[![build](https://github.com/c-cube/moonpool/actions/workflows/main.yml/badge.svg)](https://github.com/c-cube/moonpool/actions/workflows/main.yml)

A pool within a bigger pool (ie the ocean). Here, we're talking about
pools of `Thread.t` which live within a fixed pool of `Domain.t`.

This fixed pool of domains is shared between *all* the pools in moonpool.
The rationale is that we should not have more domains than cores, so
it's easier to pre-allocate exactly that many domains, and run more flexible
thread pools on top.

In addition, `Moonpool.Fut` provides futures/promises that execute
on these thread pools. The futures are thread safe.

## Usage

The user can create several thread pools. These pools use regular posix threads,
but the threads are spread across multiple domains (on OCaml 5), which enables
parallelism.

```ocaml
# let pool = Moonpool.Pool.create ~min:4 ();;
val pool : Moonpool.Pool.t = <abstr>

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
    Multiple threads have to share a single core and do not run in parallel on it[^1].
    We can therefore build pools that spread their worker threads on multiple cores to enable parallelism within each pool.

TODO: actually use https://github.com/haesbaert/ocaml-processor to pin domains to cores,
possibly optionally using `select` in dune.

## License

MIT license.

## Install

```sh, skip
$ opam install moonpool
```

[^1]: let's not talk about hyperthreading.
