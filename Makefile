build:
	ocamlbuild -use-ocamlfind -package core -tag thread -I src/ raytracer.native
	chmod +x raytracer.native
clean:
	ocamlbuild -clean
	rm out.ppm
display: build
	./raytracer.native
	display out.ppm
