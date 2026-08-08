# MLsem

MLsem is an OCaml library for typing dynamic languages using set-theoretic types.  
[Documentation](https://e-sh4rk.github.io/MLsem/doc/) - [Web version](https://e-sh4rk.github.io/MLsem/) - [Manual](https://e-sh4rk.github.io/MLsem/doc.html) - [VScode plugin](https://github.com/asmarcz/MLsem-vscode)

> [!IMPORTANT]
> This library is a research artifact and is subject to breaking changes.

## Structure

The core library part of MLsem is located in `src/lib/core/`:
- `types/*`: bindings for set-theoretic types (constructors, subtyping, tallying, etc.)
- `common/*`: auxiliary definitions (type environment, variable, etc.)
- `system/*`: functional core language (module `Ast`), type system (module `Checker`), and reconstruction algorithm (module `Reconstruction`)
- `lang/*`: full language (module `Ast`), minimal imperative language (module `MAst`) and program transformations into the functional core language

Other code directories:
- `src/lib/app`: defines the MLsem application (surface language, parser, top-level environment, etc.)
- `src/lib/lsp`: defines the LSP server application
- `src/bin`: code for the binaries (native, web version, etc.)

> [!WARNING]
> This library is not thread-safe: it must be used from a single thread of a single domain.

## Installation

The easiest way to install this library is through [opam](https://opam.ocaml.org/), the OCaml Package Manager.

This library uses algebraic effects and requires at least the version `5.3.0` of the OCaml compiler, which can be installed as follows:

```
opam switch create mlsem 5.3.0
eval $(opam env --switch=mlsem)
```

The MLsem library can then be installed as follow:
```
opam pin mlsem-types https://github.com/E-Sh4rk/MLsem.git#main
opam pin mlsem-common https://github.com/E-Sh4rk/MLsem.git#main
opam pin mlsem-system https://github.com/E-Sh4rk/MLsem.git#main
opam pin mlsem-lang https://github.com/E-Sh4rk/MLsem.git#main
opam pin mlsem https://github.com/E-Sh4rk/MLsem.git#main
```

## Building the native version

To run the native version, you can clone this repository and build MLsem as follows:
```
make deps
make
```

This will run the native version of the prototype and
type-check the definitions in the directory `tests`, where our test corpuses are.
Each corpus uses the extension `.ml` because the syntax is close to OCaml's syntax,
but it is not valid OCaml code.

## Building the Wasm version

The WebAssembly version is about 10x slower than the native version, but can be tested directly in the web browser with an interface based on [Monaco Editor](https://microsoft.github.io/monaco-editor/).  
It can be directly tested online [here](https://e-sh4rk.github.io/MLsem/) or built from sources:

```
make web-deps
make wasm
cd webeditor
python3 -m http.server 8080
```

MLsem should then be accessible from your web browser: http://localhost:8080/  
You can load examples by pressing F2 or accessing the contextual menu (right click).

## License

This software is distributed under the MIT license.
See [`LICENSE`](LICENSE) for more info.  
*This work is funded by the ERC CZ LL2325 grant.*
