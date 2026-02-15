# MLsem

Our test corpuses are in the directory `tests`.
Each corpus uses the extension `.ml` because the syntax is close to OCaml's syntax,
but it is not valid OCaml code.

## Notes for the artifact reviewers

The easiest way to test the prototype is to follow the `Testing the Wasm version (prebuilt)` section ([here](#testing-the-wasm-version-prebuilt)). The only requirement is Python 3, in order to be able to start a local http web server.
Alternatively, an online version hosted by Github is available [here](https://e-sh4rk.github.io/MLsem/).
The Wasm version being significantly slower than the native one, performance should only be measured on the [native version](#building-and-running-the-native-version).

All the code examples from the associated OOPSLA submission have been regrouped in the file `tests/4_oopsla.ml`. They are automatically type-checked when running the native version (results are printed in stdout). For the local Wasm version, they can be loaded by pressing F2 and choosing `OOPSLA`.
Claims made in the paper (types should be checked modulo commutativity of `&` and `|`):
- Types inferred for `filter` (l 63) and `filter_imp` (l 105)
- Type inferred for `neg_and_pos` (Figure 5)
- Types inferred for the examples in Figure 6
- All the examples from [Tobin-Hochstadt and Felleisen 2010] (both annotated and unannotated versions) typecheck (l 928)
- Time performance: function `bal` typechecks in about 235ms (l 974), function `filtermap` typechecks in about 15ms (l 987), function `map_no_annot` typechecks in about 30ms (l 968).

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

## Testing the Wasm version (prebuilt)

The WebAssembly version is slower than the native version, but can be tested directly in the web browser with an interface based on [Monaco Editor](https://microsoft.github.io/monaco-editor/).  

```
cd webeditor
python3 -m http.server 8080
```

MLsem should then be accessible from your web browser: http://localhost:8080/  
You can load examples by pressing F2 or accessing the contextual menu (right click).

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

## Building and running the Wasm version

The WebAssembly version can be built from sources as follows:

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
