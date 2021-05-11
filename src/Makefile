# This Makefile is only for debugging purposes of the optitrust library


ORIGIN=../tests/infra/target_debug

TEST=test.byte

all: run

origin: 
	ln -s $(ORIGIN).ml test.ml
	ln -s $(ORIGIN).cpp test.cpp

compile: force
	ocamlbuild -quiet -r -pkgs clangml,refl,pprint,str -I . $(TEST)

run: compile $(TEST)
	./$(TEST)

force:

clean:
	rm -Rf _build

clean_all: clean
	rm test.ml test.cpp
