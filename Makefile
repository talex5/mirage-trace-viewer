all: viewer

viewer:
	ocamlbuild -cflag -g -use-ocamlfind viewer.native

clean:
	ocamlbuild -clean
