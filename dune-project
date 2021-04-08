(lang dune 2.8)
(name mirage-trace-viewer)
(formatting disabled)
(generate_opam_files true)
(source (github talex5/mirage-trace-viewer))
(authors "talex5@gmail.com")
(maintainers "talex5@gmail.com")
(documentation "https://talex5.github.io/mirage-trace-viewer/")
(package
 (name mirage-trace-viewer)
 (synopsis "Viewer for CTF traces collected by mirage-profile")
 (depends
   (ocplib-endian (>= 1.1))
   (itv-tree (>= 2.1))
   (cmdliner (>= 1.0.4))
   (ocaml (>= 4.08))))
(package
 (name mirage-trace-viewer-gtk)
 (synopsis "Viewer for CTF traces collected by mirage-profile")
 (depends
  (mirage-trace-viewer (= :version))
  (lwt (>= 5.4.0))
  (lablgtk (>= 2.18.11))
  (cairo2-gtk (>= 0.6.2))
  (cairo2 (>= 0.6.2))))
(package
 (name mirage-trace-viewer-js)
 (synopsis "Viewer for CTF traces collected by mirage-profile")
 (depends
  (mirage-trace-viewer (= :version))
  (js_of_ocaml-tyxml (>= 3.9.0))
  (js_of_ocaml-ppx (>= 3.9.0))
  (js_of_ocaml-lwt (>= 3.9.0))
  (js_of_ocaml (>= 3.9.0))))
