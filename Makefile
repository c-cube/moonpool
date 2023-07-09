
DUNE_OPTS?=
build:
	dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

doc:
	@dune build $(DUNE_OPTS) @doc

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

DUNE_OPTS_BENCH?=--profile=release

N?=40
NITER?=3
BENCH_PSIZE?=1,4,8,20
BENCH_CUTOFF?=20
bench-fib:
	@echo running for N=$(N)
	dune build $(DUNE_OPTS_BENCH) benchs/fib_rec.exe
	hyperfine -L psize $(BENCH_PSIZE) \
		'./_build/default/benchs/fib_rec.exe -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize={psize} -n $(N)'

PI_NSTEPS?=100_000_000
PI_MODES?=seq,par1
bench-pi:
	@echo running for N=$(PI_NSTEPS)
	dune build $(DUNE_OPTS_BENCH) benchs/pi.exe
	hyperfine -L mode $(PI_MODES) \
		'./_build/default/benchs/pi.exe -mode={mode} -n $(PI_NSTEPS)'

.PHONY: test clean bench-fib bench-pi
