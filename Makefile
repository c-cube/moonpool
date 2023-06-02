
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

.PHONY: test clean
