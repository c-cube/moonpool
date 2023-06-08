
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
NITER?=3
BENCH_PSIZE?=1,4,8,20
bench-fib:
	@echo running for N=$(N)
	dune build $(DUNE_OPTS_BENCH) benchs/fib_rec.exe
	hyperfine -L psize $(BENCH_PSIZE) \
		'./_build/default/benchs/fib_rec.exe -niter $(NITER) -psize={psize} -n $(N)'

.PHONY: test clean
