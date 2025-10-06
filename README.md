# MLsem

Our test corpus is in the file `test.ml`.
It uses the extension `.ml` because the syntax is close to OCaml's syntax,
but it is not valid OCaml code. 

## Testing the Wasm version (prebuilt)

The WebAssembly version is about 10x slower than the native version, but can be tested directly in the web browser with an interface based on [Monaco Editor](https://microsoft.github.io/monaco-editor/).  

```
cd webeditor
python3 -m http.server 8080
```

MLsem should then be accessible from your web browser: http://localhost:8080/  
You can load examples by pressing F2 or accessing the contextual menu (right click).

## Building and running the native/Wasm version

The [OCaml Package Manager](https://opam.ocaml.org/) must be installed first.

```
opam switch create mlsem 5.3.0
eval $(opam env --switch=mlsem)
opam install dune
opam install . --deps-only
make
```

This will run the native version of the prototype and
type-check the definitions in `test.ml`.

The WebAssembly version can also be built from sources:

```
make wasm
```

This will replace the prebuilt version of `typechecker.js` and `wasm.bc.wasm.assets` in the directory `webeditor`.