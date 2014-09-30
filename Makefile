all:
	ocamlbuild -use-ocamlfind sim.native
	./sim.native

clean:
	ocamlbuild -clean
