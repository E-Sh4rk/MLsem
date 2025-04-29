
all: build run

deps:
	opam install . --deps-only

build:
	opam exec -- dune build src/main/prototype.exe

run:
	opam exec -- dune exec ./src/main/prototype.exe

clean:
	opam exec -- dune clean
	rm -f ./webeditor/typechecker.js ./webeditor/version.txt

libjs:
	opam exec -- dune build --profile release src/main/lib_js.bc.js
	cp _build/default/src/main/lib_js.bc.js ./webeditor/typechecker.js
	chmod +w ./webeditor/typechecker.js
	git describe --always --tags HEAD > ./webeditor/version.txt
	chmod +w ./webeditor/version.txt

test:
	opam exec -- dune runtest

perf:
	sudo perf record --call-graph=dwarf -- ./_build/default/src/main/prototype.exe
	sudo perf report
