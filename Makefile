
DUNE_OPTS?=
build:
	dune build @install $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

doc:
	@dune build $(DUNE_OPTS) @doc

build-dev:
	dune build @install @runtest $(DUNE_OPTS) --workspace=dune-workspace.dev

WATCH?= @check @runtest
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

DUNE_OPTS_BENCH?=--profile=release

N?=40
NITER?=2
BENCH_PSIZE?=1,4,8,20
BENCH_KIND?=fifo,pool
BENCH_CUTOFF?=20
bench-fib:
	@echo running for N=$(N)
	dune build $(DUNE_OPTS_BENCH) benchs/fib_rec.exe

	hyperfine --warmup=1 \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -seq' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -dl' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize=20 -kind=pool -fj' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize=20 -kind=pool -await' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize=4 -kind=fifo' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize=4 -kind=pool' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize=8 -kind=fifo' \
      './_build/default/benchs/fib_rec.exe -n $(N) -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize=16 -kind=pool'

	#hyperfine -L psize $(BENCH_PSIZE) -L kind $(BENCH_KIND) --warmup=1 \
	#	'./_build/default/benchs/fib_rec.exe -cutoff $(BENCH_CUTOFF) -niter $(NITER) -psize={psize} -kind={kind} -n $(N)'
		#'./_build/default/benchs/fib_rec.exe -seq -cutoff $(BENCH_CUTOFF) -niter $(NITER) -n $(N)' \
		#'./_build/default/benchs/fib_rec.exe -dl -cutoff $(BENCH_CUTOFF) -niter $(NITER) -n $(N)' \

PI_NSTEPS?=100_000_000
PI_MODES?=seq,par1,forkjoin
PI_KIND?=fifo,pool
bench-pi:
	@echo running for N=$(PI_NSTEPS)
	dune build $(DUNE_OPTS_BENCH) benchs/pi.exe
	hyperfine --warmup=1 \
		'./_build/default/benchs/pi.exe -n $(PI_NSTEPS) -mode=seq' \
		'./_build/default/benchs/pi.exe -n $(PI_NSTEPS) -j 8 -mode par1 -kind=pool' \
		'./_build/default/benchs/pi.exe -n $(PI_NSTEPS) -j 8 -mode par1 -kind=fifo' \
		'./_build/default/benchs/pi.exe -n $(PI_NSTEPS) -j 16 -mode forkjoin -kind=pool' \
		'./_build/default/benchs/pi.exe -n $(PI_NSTEPS) -j 20 -mode forkjoin -kind=pool'

.PHONY: test clean bench-fib bench-pi

VERSION=$(shell awk '/^version:/ {print $$2}' moonpool.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/**/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.mli)
