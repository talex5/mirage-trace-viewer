all: gtk_viewer html_viewer

gtk_viewer:
	ocamlbuild -cflag -g -use-ocamlfind gtk_viewer.native

html_viewer:
	ocamlbuild -cflag -g -use-ocamlfind html_viewer.byte precompute.native
	./precompute.native examples/log-x86.sexp
	js_of_ocaml --opt=3 +weak.js html_viewer.byte -I examples --file log-x86.bin

clean:
	ocamlbuild -clean
