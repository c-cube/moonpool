build:
	ocamlbuild -use-ocamlfind -package core -tag thread raytracer.native
display: build
	./raytracer.native > foo.ppm
	display foo.ppm
