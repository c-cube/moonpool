#!/bin/sh
DUNE_OPTS="--profile=release --display=quiet"
sudo dune exec $DUNE_OPTS benchs/raytracer/raytracer.exe -- $@
