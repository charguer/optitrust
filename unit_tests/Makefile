#######################################################
# Usage:
#    `make`          attempts to build all the CHECKS
#    `make foo.out`  to execute the test `foo`
#    `make foo.chk`  to check the result of the test `foo` against expected result
#    `make V=`       for more verbose output
#    `make VC=`      for more verbose output only for checking C++ compilation
#    `make f`        force rebuilding all (short for "make clean; make")
#
# foo.ml         describes a transformation on foo.cpp
# foo.cpp        the source file for the transformation
# foo_out.cpp    the output produced by the transformation
# foo_exp.cpp    the expected output
# foo.chk        an empty file created if foo_out.cpp match foo_exp.cpp
# 


############################################# stat##########
# How to create a new test
# 
# # If the transformation is already implemented
#  r
# gedit ${NAME}.{ml,cpp} &
# make ${NAME}.exp
# git add ${NAME}{.ml,.cpp,_exp.cpp}
#
# 
# # If the transformation is not yet implemented
# NAME=testname
# gedit ${NAME}{.cpp,_exp.cpp} &
# git add ${NAME}{.cpp,_exp.cpp}
# 


#######################################################
# Parameters

# List of tests that are not ready to include in the benchmark
EXCLUDE_ML=label_add.ml label_rem.ml er
V?=@

# verbosity of C file checking
VC?=no

#######################################################
# File lists

# TESTS contains the list of ml transformation scripts
TESTS=$(filter-out $(EXCLUDE_ML), $(wildcard *.ml))

# CHECKS contains the list of targets to be produced
CHECKS=$(TESTS:.ml=.chk)

# The build command for compiling a script
BUILD=ocamlbuild -quiet -pkgs clangml,refl,pprint,str,optiTrust.scriptTools

# List of cpp files found in the folder
CPP_SOURCES=$(filter-out $(EXCLUDE_CPP), $(wildcard *.cpp))


#######################################################
# Targets

# 'make all' runs each of the unit tests
all: transfo compile

transfo: $(CHECKS)

# 'make f' forces clean before 'make all'
f: clean all

# make 'compile' checks that the cpp files commited and generated all compile
compile: $(CPP_SOURCES:.cpp=.prog) $(TESTS:.ml=_out.prog)


#######################################################
# Rules

# Instruction to keep intermediate files 
.PRECIOUS: %.byte %_out.cpp %.chk

# Rule for executing one given test and comparing it to the expected result
%.out: %_out.cpp %_exp.cpp
	$(V)diff -q --ignore-space-change $^ && echo "===> Matches expected output <===" || true
	$(V)cat $<

# Rule for building .chk: compare the output and the expected output
%.chk: %_out.cpp %_exp.cpp 
	$(V) (diff -q --ignore-space-change $^ > /dev/null && touch $@ && echo "$< matches the expected result") \
	|| (echo "$< does not match the expected result:" && diff --ignore-space-change $^)

# Rule for building the output of a test: build the binary and run it; result depends on input .cpp file
%_out.cpp: %.byte %.cpp
	$(V)./$<

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

# LATER: we might want to activate more warnings, e.g.
# MOREWARNINGS=-Wall -Wno-unused-variable -Wunused-but-set-variable 


#######################################################
# Cleanup

clean:
	$(V)rm -rf *_out.cpp *.byte *.chk *.log *.ast *.out *.prog
	$(V)rm -rf _build
	@echo "Clean successful"

#######################################################

# LATER: it would be nice that the rule for building %.byte has a dependency on
# the opam library ScriptTools, so that after a "make install" we are not forced to
# do a "make clean" by hand in this folder.




