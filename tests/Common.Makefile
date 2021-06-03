#######################################################
# Usage:
#    `make`          attempts to build all the *.chk files
#    `make foo.out`  to execute the test `foo` and compare with expected result
#    `make foo.res`  to execute the test `foo` and compared with source file
#    `make foo.chk`  to check the result of the test `foo` against expected result
#    `make V=`       for more verbose output
#    `make VC=`      for more verbose output only for checking C++ compilation
#    `make f`        force rebuilding all (short for "make clean; make")
#    `make foo.meld` opens meld to compare the output with the expected output 
#
#         tip: in meld, activate: file / preference / text filters / all whitespace
#
# foo.ml         describes a transformation on foo.cpp
# foo.cpp        the source file for the transformation
# foo_out.cpp    the output produced by the transformation
# foo_exp.cpp    the expected output
# foo.chk        an empty file created if foo_out.cpp match foo_exp.cpp


#######################################################
# Parameters

# Verbosity for commands
V ?= @

# verbosity of C file checking
VC ?= no

# List of ml files to not include in the list of tests to consider (by default all *.ml are considered)
EXCLUDE_TESTS ?=

# List of ml files to include (by default, all *.ml except those in EXCLUDE_TESTS)
TESTS ?= $(filter-out $(EXCLUDE_TESTS), $(wildcard *.ml))

# Path to the folder containing optitrust main Makefile, on which to call make install
OPTITRUST ?= ../..


#######################################################
# Targets

# 'make all' runs each of the unit tests (execute the transformation, and compile all c code)
all: transfo compile

# 'make f' forces clean before 'make all'
f: clean all

# 'make transfo' executes all the transformations
transfo: $(TESTS:.ml=.chk)

# make 'compile' checks that the cpp files commited and generated all compile
compile: $(TESTS:.ml=.prog)

# 'make optitrust' rebuilds the library, and clean all local files
optitrust: clean
	$(MAKE) -C $(OPTITRUST) install


#######################################################
# Rules

# Instruction to keep intermediate files 
.PRECIOUS: %.byte %_out.cpp %.chk

# Rule for executing one given test
%.res: %_out.cpp
	$(V)cat $<
	@echo "\n============================="
	$(V)cat $*_out_enc.cpp

# The command for calling diff
DIFF := diff --ignore-blank-lines --ignore-space-change -I '^//'

# Rule for executing one given test and comparing it to the expected result
# TODO: should use $(DIFF)? here
%.out: %_out.cpp
	$(V)diff -q --ignore-space-change $^ `basename -s _out.cpp $<`_exp.cpp && echo "===> Matches expected output <===" || true
	$(V)cat $<

# Rule for building .chk: compare the output and the expected output
%.chk: %_out.cpp %_exp.cpp 
	$(V) ($(DIFF) -q $^ > /dev/null && touch $@ && echo "$< matches the expected result") \
	|| (echo "$< does not match the expected result:" && echo "  make $*.meld")
#	|| (echo "$< does not match the expected result:" && $(DIFF) $^)

# Rule for building the output of a test: build the binary and run it; result depends on input .cpp file
%_out.cpp: %.byte %.cpp
	$(V)./$<
	
# The build command for compiling a script
BUILD=ocamlbuild -quiet -pkgs clangml,refl,pprint,str,optiTrust.optitrust

# Rule for building the binary associated with a test
%.byte: %.ml
	$(V)$(BUILD) $@ 

# Rule for producing the expected output file from the result
%.exp: %_out.cpp
	$(V)cp $< `basename -s .exp $@`_exp.cpp
	@echo "Generated `basename -s .exp $@`_exp.cpp from $<"

# Rule for checking that a file compiles
%.prog: %.cpp
	@gcc -c -std=c++11 $< -o $@
ifeq ($(VC),)
	$(VC)@echo "Compiled $< successfully"
endif

# Rule for opening meld to compare the output and the expected output
%.meld: %_out.cpp %_exp.cpp
	meld $^

# LATER: we might want to activate more warnings, e.g.
# MOREWARNINGS=-Wall -Wno-unused-variable -Wunused-but-set-variable 


#######################################################
# Documentation

# CHECKS contains the list of targets to be produced for the documentation
DIFFJS=$(TESTS:.ml=_diff.js)

# Rule for producing the diff between the output and the expected output, in a form readable in a browser
%_diff.js: %.cpp %_exp.cpp %.ml
	@echo "function get_diff_$*() { return window.atob(\"`git diff --no-index -U10 $*.cpp $*_exp.cpp | base64 -w 0`\"); }" > $@
	@echo "function get_src_$*() { return window.atob(\"`cat $*.ml | base64 -w 0`\"); }" >> $@
	@echo Produced $@

# 'make doc' to build the auxililary files needed by _doc.html
doc: $(DIFFJS)

# 'make redoc' to force rebuilding all *_diff.js files
redoc:
	rm -f *_diff.js
	$(MAKE) doc

# 'make opendoc' to view the documentation
opendoc: doc
	mkdir -p .chromium
	chromium-browser --new-window --user-data-dir=../.chromium --disable-web-security _doc.html

#######################################################
# Cleanup

clean:
	$(V)rm -rf *_out.cpp *.byte *.chk *.log *.ast *.out *.prog *_enc.cpp *_diff.js *_before.cpp *_after.cpp *_diff.html *_with_exit.ml
	$(V)rm -rf _build
	@echo "Clean successful"


