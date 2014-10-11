all: gtk_viewer html_viewer

gtk_viewer:
	ocamlbuild -cflag -g -use-ocamlfind gtk_viewer.native

html_viewer:
	ocamlbuild -cflag -g -use-ocamlfind html_viewer.byte
	js_of_ocaml +weak.js html_viewer.byte

clean:
	ocamlbuild -clean
