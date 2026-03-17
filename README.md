# Artifact for the paper "Revisiting Row Polymorphism for Set-Theoretic Types"

This artifact is a modified version of the MLsem typechecker implementing polymorphic records.
The original MLsem typechecker is released under MIT license.

## Running the web prototype

A WebAssembly version can be tested directly in the web browser.
This version is slower than the native version.

```
cd webeditor
python3 -m http.server 8080
```

MLsem should then be accessible from your web browser: http://localhost:8080/

You can load examples by pressing F2 or accessing the contextual menu (right click).
Examples of the paper are available under the category "Row Polymorphism".

## Native version installation

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create sstt 5.3.0
eval $(opam env --switch=sstt)
```

The native version can then be built as follows:
```
make deps
make
```
