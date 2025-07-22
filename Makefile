
all: build run

deps:
	opam install . --deps-only

web-deps:
	opam install js_of_ocaml js_of_ocaml-ppx wasm_of_ocaml-compiler
	cd webeditor ; npm ci

build:
	opam exec -- dune build src/main/prototype.exe

run:
	opam exec -- dune exec ./src/main/prototype.exe

clean:
	opam exec -- dune clean
	rm -f ./webeditor/typechecker.js ./webeditor/version.txt

js:
	opam exec -- dune build --profile release src/main/js.bc.js
	cp _build/default/src/main/js.bc.js ./webeditor/typechecker.js
	chmod +w ./webeditor/typechecker.js
	git describe --always --tags HEAD > ./webeditor/version.txt
	chmod +w ./webeditor/version.txt

wasm:
	opam exec -- dune build --profile release src/main/wasm.bc.wasm.js
	cp _build/default/src/main/wasm.bc.wasm.js ./webeditor/typechecker.js
	cp -r _build/default/src/main/wasm.bc.wasm.assets ./webeditor/
	chmod +w ./webeditor/typechecker.js ./webeditor/wasm.bc.wasm.assets ./webeditor/wasm.bc.wasm.assets/*
	git describe --always --tags HEAD > ./webeditor/version.txt
	chmod +w ./webeditor/version.txt

test:
	opam exec -- dune runtest

time:
	./time.sh

perf:
	sudo perf record --call-graph=dwarf -- ./_build/default/src/main/prototype.exe
	sudo perf report
