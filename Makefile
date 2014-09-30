all:
	ocamlbuild -cflag -g -use-ocamlfind sim.native
	OCAMLRUNPARAM=b ./sim.native

clean:
	ocamlbuild -clean
