# TODO: Be more clever about when to rebuild

all: build

build:
	ocamlbuild -use-ocamlfind optitrust_runner.native

rebuild: clean build

clean:
	rm -rf _build

.PHONY: build clean
