(lang dune 3.17)
(using menhir 3.0)

(name mlsem)

(generate_opam_files true)

(authors "Mickael Laurent")

(maintainers "Mickael Laurent")

(homepage "https://github.com/E-Sh4rk/MLsem")

(bug_reports "https://github.com/E-Sh4rk/MLsem/issues")

(license "MIT")

(package
 (name mlsem)
 (synopsis "A prototype type-checker for dynamic languages")
 (depends (ocaml (>= 5.3.0)) dune (sstt (>= 0.1.0)) ppx_deriving zarith yojson js_of_ocaml-ppx zarith_stubs_js)
)
