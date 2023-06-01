
DUNE_OPTS?=
all:
	dune build @all $(DUNE_OPTS)

clean:
	@dune clean

test:
	@dune runtest $(DUNE_OPTS)

WATCH?=@all
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)

.PHONY: test clean
