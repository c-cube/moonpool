[API reference](https://ocaml-multicore.github.io/backoff/doc/backoff/Backoff/index.html)

# backoff - exponential backoff mechanism

**backoff** provides an
[exponential backoff mechanism](https://en.wikipedia.org/wiki/Exponential_backoff)
[1]. It reduces contention by making a domain back off after failing an
operation contested by another domain, like acquiring a lock or performing a
`CAS` operation.

## About contention

Contention is what happens when multiple CPU cores try to access the same
location(s) in parallel. Let's take the example of multiple CPU cores trying to
perform a `CAS` on the same location at the same time. Only one is going to
success at each round of retries. By writing on a shared location, it
invalidates all other CPUs' caches. So at each round each CPU will have to read
the memory location again, leading to quadratic O(n²) bus traffic.

## Exponential backoff

Failing to access a shared resource means there is contention: some other CPU
cores are trying to access it at the same time. To avoid quadratic bus traffic,
the idea exploited by exponential backoff is to make each CPU core wait (spin) a
random bit before retrying. This way, they will try to access the resource at a
different time: that not only strongly decreases bus traffic but that also gets
them a better chance to get the resource, at they probably will compete for it
against less other CPU cores. Failing again probably means contention is high,
and they need to wait longer. In fact, each consecutive fail of a single CPU
core will make it wait twice longer (_exponential_ backoff !).

Obviously, they cannot wait forever: there is an upper limit on the number of
times the initial waiting time can be doubled (see [Tuning](#tuning)), but
intuitively, a good waiting time should be at least around the time the
contested operation takes (in our example, the operation is a CAS) and at most a
few times that amount.

## Tuning

For better performance, backoff can be tuned. `Backoff.create` function has two
optional arguments for that: `upper_wait_log` and `lower_wait_log` that defines
the logarithmic upper and lower bound on the number of spins executed by
{!once}.

## Drawbacks

This mechanism has some drawbacks. First, it adds some delays: for example, when
a domain releases a contended lock, another domain, that has backed off after
failing acquiring it, will still have to finish its back-off loop before
retrying. Second, this increases any unfairness: any other thread that arrives
at that time or that has failed acquiring the lock for a lesser number of times
is more likely to acquire it as it will probably have a shorter waiting time.

## Example

To illustrate how to use backoff, here is a small implementation of
`test and test-and-set` spin lock [2].

```ocaml
  type t = bool Atomic.t

  let create () = Atomic.make false

  let rec acquire ?(backoff = Backoff.detault) t =
    if Atomic.get t then begin
      Domain.cpu_relax ();
      acquire ~backoff t
    end
    else if not (Atomic.compare_and_set t false true) then
      acquire ~backoff:(Backoff.once backoff) t

  let release t = Atomic.set t false
```

This implementation can also be found [here](bench/taslock.ml), as well as a
small [benchmark](bench/test_tas.ml) to compare it to the same TAS lock but
without backoff. It can be launched with:

```sh
dune exec ./bench/test_tas.exe > bench.data
```

and displayed (on linux) with:

```sh
gnuplot -p -e 'plot for [col=2:4] "bench.data" using 1:col with lines title columnheader'
```

## References

[1] Adaptive backoff synchronization techniques, A. Agarwal, M. Cherian (1989)

[2] Dynamic Decentralized Cache Schemes for MIMD Parallel Processors, L.Rudolf,
Z.Segall (1984)
