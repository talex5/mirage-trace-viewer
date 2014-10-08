viewer:
	ocamlbuild -cflag -g -use-ocamlfind viewer.native

all:
	ocamlbuild -cflag -g -use-ocamlfind sim.native viewer.native
	OCAMLRUNPARAM=b ./sim.native
	#OCAMLRUNPARAM=b ./viewer.native

clean:
	ocamlbuild -clean
