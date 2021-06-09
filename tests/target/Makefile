# This Makefile is only for debugging purposes of the optitrust library
#
# Usage:
# 1. cd to tests/target
# 2. set "TEST" to the filename without extension (e.g. "target_debug")
# 3. start the program .vscode/watch.sh
# 4. in any file from src/, type F6
# 5. this calls .vscode/debug_one_test.sh
# 6. which calls "make run" in this folder
# 7. which recompiles optitrust, and the test file, and executes it

# Basename of the test case to consider
TEST=target_debug

# Path to the optitrust library
OPTITRUST = ../..

# Name of the test program
PROG=$(TEST).byte

# Default target is "run"
all: run

# Target to rebuild the library and the test and execute the test
run: optitrust exec

# Target to rebuild the library, forces a clean up of the generated files for safety
optitrust: clean
	$(MAKE) -C $(OPTITRUST) install

# Target to build the test program
$(PROG): force
	ocamlbuild -tag debug -quiet -r -pkgs clangml,refl,pprint,str,optitrust -I . $(PROG)

# Target to execute the test program
exec: $(PROG)
	bash -c "OCAMLRUNPARAM=b ./$(PROG) 1> >( sed -u 's/\\]:/\\]-/g') 2> >( sed -u 's/file \\\"\\([^,]*\\)\\\", line \\([0-9]*\\),/\\1:\\2:/g' >&2)" && cat test_out.cpp && echo ""

# Auxiliary target to force the reconstruction of the test program after an update to the library
force:

# Target to clean the generated files
clean:
	rm -Rf _build
	rm -f $(TEST).byte $(TEST).log $(TEST)_out.ast $(TEST)_out.cpp $(TEST)_out_enc.cpp

