
DUNE_OPTS?=
all:
	dune build @all $(DUNE_OPTS)

clean:
	@dune clean

WATCH?=@all
watch:
	dune build $(DUNE_OPTS) -w $(WATCH)
