
DUNE_OPTS?=
all:
	dune build @all $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

doc:
	@dune build $(DUNE_OPTS) @doc

WATCH?=@all
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

DUNE_OPTS_BENCH?=--profile=release

N?=40
bench-fib:
	@echo running for N=$(N)
	dune build $(DUNE_OPTS_BENCH) benchs/fib_rec.exe
	hyperfine \
		'./_build/default/benchs/fib_rec.exe -psize=1 -n $(N)' \
		'./_build/default/benchs/fib_rec.exe -psize=8 -n $(N)' \
		'./_build/default/benchs/fib_rec.exe -psize=16 -n $(N)' \
		'./_build/default/benchs/fib_rec.exe -n $(N) -seq'

.PHONY: test clean
