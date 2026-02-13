# MLsem

Our test corpuses are in the directory `tests`.
Each corpus uses the extension `.ml` because the syntax is close to OCaml's syntax,
but it is not valid OCaml code.

## Documentation

The core of MLsem is located in `src/lib/core/`:
- `types/*`: bindings for set-theoretic types (constructors, subtyping, tallying, etc.)
- `common/*`: auxiliary definitions (type environment, variable, etc.)
- `system/*`: functional core language (module `Ast`), type system (module `Checker`), and reconstruction algorithm (module `Reconstruction`)
- `lang/*`: full language (module `Ast`), minimal imperative language (module `MAst`) and program transformations into the functional core language

Documentation can be accessed [here](https://e-sh4rk.github.io/MLsem/doc/).
It can also be generated from source:

```
opam install odoc
make doc
```

This will generate the documentation in `webeditor/doc/`.

## Building and running the native version

The [OCaml Package Manager](https://opam.ocaml.org/) must be installed first.

```
opam switch create mlsem 5.3.0
eval $(opam env --switch=mlsem)
make deps
make
```

This will run the native version of the prototype and
type-check the definitions in the directory `tests`.

## Testing the Wasm version

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
