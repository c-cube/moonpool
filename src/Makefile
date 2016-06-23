build:
	ocamlbuild -use-ocamlfind -package core -tag thread raytracer.native
display: build
	./raytracer.native
	display matte.ppm
