# Artifact for the paper "Revisiting Row Polymorphism for Set-Theoretic Types"

This directory contains the source code for the type-checker MLsem with support for row-polymorphism.
To build it and test it locally, follow the section [native version](#native-version).
Alternatively, a Wasm version hosted by Github is available [here](https://e-sh4rk.github.io/MLsem/).
Note that the Wasm version is significantly slower than the native one.

MLsem language documentation: https://e-sh4rk.github.io/MLsem/doc.html (or `webeditor/doc.html`)  
Source code documentation: https://e-sh4rk.github.io/MLsem/doc/ (or `make doc` to generate)

Our test corpuses are in the directory `tests`, but only `tests/artifacts/rowpoly-oopsla26.ml` is relevant to this paper.
Each corpus uses the extension `.ml` because the syntax is close to OCaml's syntax,
but it is not valid OCaml code.

Claims made in the paper:
- Types inferred for the examples in Appendix A, which can also be found in `tests/artifacts/rowpoly-oopsla26.ml`.

## Native version

The [OCaml Package Manager](https://opam.ocaml.org/) must be installed first.

Initialize an OPAM environment of compiler version 5.3.0 (or you can reuse the one created for SSTT):
```
opam switch create mlsem 5.3.0
eval $(opam env --switch=mlsem)
```

You can then build and run the native version as follows:
```
make deps
make build
opam exec -- dune exec ./src/bin/native.exe tests/artifacts/rowpoly-oopsla26.ml
```

## Web version

A WebAssembly version can be built and tested directly in the web browser.
This version is slower than the native version.

```
make web-deps
make wasm
cd webeditor
python3 -m http.server 8080
```

MLsem should then be accessible from your web browser: http://localhost:8080/

A prebuilt Wasm version is available on Github: https://e-sh4rk.github.io/MLsem/
