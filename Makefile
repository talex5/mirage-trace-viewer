all: gtk_viewer html_viewer html_example.js

gtk_viewer:
	ocamlbuild -cflag -g -use-ocamlfind gtk_viewer.native

html_viewer:
	ocamlbuild -cflag -g -use-ocamlfind html_viewer.cmo

html_example.js:
	ocamlbuild -cflag -g -use-ocamlfind example_html.byte precompute.native
	./precompute.native examples/log-x86.sexp
	js_of_ocaml --opt=3 +weak.js example_html.byte -I examples --file log-x86.bin

clean:
	ocamlbuild -clean
