#!/bin/sh
OPTS="--profile=release --display=quiet"
exec dune exec $OPTS -- benchs/primes.exe $@
