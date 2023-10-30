#!/bin/sh
exec hyperfine --warmup=1 './bench_fib.sh -cutoff 15 -n 42 -niter 2 -psize=20 -kind=pool -fj' './bench_fib.sh -cutoff 15 -n 42 -niter 2 -psize=20 -kind=pool -await' './bench_fib.sh -cutoff 15 -n 42 -niter 2 -dl'
