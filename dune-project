(lang dune 3.17)
(using menhir 3.0)

(name mlsem)

(generate_opam_files true)

(authors "Anonymized")

(maintainers "Anonymized")

(homepage "Anonymized")

(bug_reports "Anonymized")

(license "MIT")

(package
 (name mlsem-types)
 (synopsis "Type bindings for MLsem")
 (depends (ocaml (>= 5.3.0)) dune zarith)
)

(package
 (name mlsem-common)
 (synopsis "Common definitions for MLsem")
 (depends (ocaml (>= 5.3.0)) dune mlsem-types ppx_deriving)
)

(package
 (name mlsem-system)
 (synopsis "Type-checker for MLsem")
 (depends (ocaml (>= 5.3.0)) dune mlsem-types mlsem-common ppx_deriving)
)

(package
 (name mlsem-lang)
 (synopsis "Language for MLsem")
 (depends (ocaml (>= 5.3.0)) dune mlsem-types mlsem-common mlsem-system ppx_deriving)
)

(package
 (name mlsem)
 (synopsis "A type-checker for dynamic languages")
 (depends (ocaml (>= 5.3.0)) dune mlsem-types mlsem-common mlsem-system mlsem-lang)
)

(package
 (name mlsem-app)
 (synopsis "A statically typed dynamic languages")
 (depends mlsem menhir yojson)
)

(package
 (name mlsem-bin)
 (synopsis "Binaries for mlsem-app")
 (depends mlsem-app)
)
