#!/bin/bash -eux
./configure --enable-xen
make
mkdir ${DISTDIR}/bin
mkdir ${DISTDIR}/gtk
mkdir ${DISTDIR}/xen
cp README.md ${DISTDIR}/
cp LICENSE ${DISTDIR}/
cp _build/gtk/mtv-gtk-plugin.cmxs ${DISTDIR}/gtk/
cp _build/xen/mtv-xen-plugin.cmxs ${DISTDIR}/xen/
cp _build/main/main.native ${DISTDIR}/bin/mirage-trace-viewer
