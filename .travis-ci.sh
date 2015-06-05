#!/bin/bash -eux
# Install OCaml and OPAM PPAs
case "$OCAML_VERSION" in
  4.01.0) ppa=avsm/ocaml41+opam12 ;;
  4.02.0) ppa=avsm/ocaml42+opam12 ;;
  *) echo Unknown $OCAML_VERSION; exit 1 ;;
esac

echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam time libgmp-dev pkg-config

echo OCaml version
ocaml -version

export OPAMYES=1

opam init git://github.com/ocaml/opam-repository
eval `opam config env`
opam update
sudo apt-get install libxen-dev libgtk2.0-dev
opam install ocamlfind itv-tree ocplib-endian cmdliner js_of_ocaml xen-gnt xenstore_transport lablgtk cairo2 mirage-profile tyxml react reactiveData

./configure --prefix=`opam config var prefix`
make
make install
mirage-trace-viewer --help | cat
