#!/bin/sh
OPTS="--profile=release --display=quiet"
exec dune exec $OPTS -- benchs/fib_rec.exe $@
