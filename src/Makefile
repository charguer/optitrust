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
	./$(TEST) && cat test_out.cpp

force:

clean:
	rm -Rf _build

clean_res:
	rm test.byte test.cpp test.log test.ml test_out.ast test_out.cpp test_out_enc.cpp

clean_all: clean clean_res
	rm test.ml test.cpp 

