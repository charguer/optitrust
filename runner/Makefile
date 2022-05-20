# OPTITRUSTLIB := $(shell command -v ocamlfind > /dev/null  && ocamlfind query optitrust)
# later clever dependencies

all: build

build: clean
	ocamlbuild -use-ocamlfind optitrust_runner.native

optitrust:
	make -C .. install_lib

clean:
	rm -rf _build *.native

.PHONY: build clean
