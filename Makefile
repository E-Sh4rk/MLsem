
all: build run

deps:
	opam install . --deps-only

web-deps:
	opam install js_of_ocaml js_of_ocaml-ppx wasm_of_ocaml-compiler
	cd webeditor ; npm ci

build:
	opam exec -- dune build src/bin/native.exe

doc:
	opam exec -- dune build @doc
	rm -rf webeditor/doc
	cp -r _build/default/_doc/_html/ webeditor/doc

run:
	opam exec -- dune exec ./src/bin/native.exe test.ml

clean:
	opam exec -- dune clean

js:
	opam exec -- dune build --profile release src/bin/js.bc.js
	cp _build/default/src/bin/js.bc.js ./webeditor/typechecker.js
	chmod +w ./webeditor/typechecker.js
	git describe --always --tags HEAD > ./webeditor/version.txt
	chmod +w ./webeditor/version.txt

wasm:
	opam exec -- dune build --profile release src/bin/wasm.bc.wasm.js
	cp _build/default/src/bin/wasm.bc.wasm.js ./webeditor/typechecker.js
	cp -r _build/default/src/bin/wasm.bc.wasm.assets ./webeditor/
	chmod +w ./webeditor/typechecker.js ./webeditor/wasm.bc.wasm.assets ./webeditor/wasm.bc.wasm.assets/*
	git describe --always --tags HEAD > ./webeditor/version.txt
	chmod +w ./webeditor/version.txt

test:
	opam exec -- dune runtest

time:
	./time.sh

perf:
	sudo perf record --call-graph=dwarf -- ./_build/default/src/bin/native.exe test.ml
	sudo perf report
