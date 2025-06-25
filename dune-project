(lang dune 3.14)

(using menhir 3.0)

(name bdfparser)

(generate_opam_files true)

(source
 (github mdales/bdfparser))

(authors "Michael Dales <michael@digitalflapjack.com>")

(maintainers "Michael Dales <michael@digitalflapjack.com>")

(license ICS)

(package
  (name        bdfparser)
  (synopsis    "Library to parse BDF font files.")
  (description "Library to parse BDF font files.")
  (depends
   ocaml
   dune
   menhir
   ppx_sexp_conv
   (ounit :with-test))
  (tags (font rendering bitmap)))
