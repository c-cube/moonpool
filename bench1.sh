#!/bin/sh
exec dune exec --release --display=quiet -- test/t_bench1.exe $@
