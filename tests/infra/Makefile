# This Makefile is only for debugging purposes of the optitrust library
#
# Usage:
# 0. start the program .vscode/watch.sh
# 1. cd to tests/infra
# 2. set "ORIGIN" in this makefile to the file to debug
# 3. execute "make origin"
# 4. in any file from src/, type F6
# 5. this calls .vscode/debug_one_test.sh
# 6. which calls "make run" in this folder
# 7. which recompiles optitrust, and the test, and runs it


OPTITRUST = ../..

ORIGIN=target_debug

TEST=test.byte

all: run


origin:
	ln -s $(ORIGIN).ml test.ml
	ln -s $(ORIGIN).cpp test.cpp

$(TEST): force
	ocamlbuild -tag debug -quiet -r -pkgs clangml,refl,pprint,str,optiTrust.optitrust -I . $(TEST)

force:

clean:
	rm -Rf _build

optitrust: clean
	$(MAKE) -C $(OPTITRUST) install

run: optitrust exec

exec: $(TEST)
	bash -c "OCAMLRUNPARAM=b ./$(TEST) 1> >( sed -u 's/\\]:/\\]-/g') 2> >( sed -u 's/file \\\"\\([^,]*\\)\\\", line \\([0-9]*\\),/\\1:\\2:/g' >&2)" && cat test_out.cpp && echo ""


clean_res:
	rm test.byte test.cpp test.log test.ml test_out.ast test_out.cpp test_out_enc.cpp

clean_all: clean clean_res
	rm test.ml test.cpp

