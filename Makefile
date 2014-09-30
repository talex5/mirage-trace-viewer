all:
	ocamlbuild -use-ocamlfind sim.native
	./sim.native
	dot -Tpng graph.dot > graph.png

clean:
	ocamlbuild -clean
