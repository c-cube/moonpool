
FROM ocaml/opam:debian-unstable-ocaml-5.0 as base
RUN cd /home/opam/opam-repository; git fetch origin; git checkout 01487021cbdc2c54d643dbf5696e2211ca3db31d -q; opam upd
WORKDIR /home/opam/
COPY *.opam dune-project ./
COPY src src
COPY test test
RUN eval `opam env` && opam install . --deps-only -t
RUN eval `opam env` && dune build @install
RUN eval `opam env` && dune runtest
