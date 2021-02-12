#######################################################
# Usage:
#    `make`          attempts to build all the CHECKS
#    `make foo.out`  to execute the test `foo`
#    `make foo.chk`  to check the result of the test `foo` against expected result
#    `make V=`       for more verbose output
#
# foo.ml         describes a transformation on foo.cpp
# foo.cpp        the source file for the transformation
# foo_out.cpp    the output produced by the transformation
# foo_exp.cpp    the expected output
# foo.chk        an empty file created if foo_out.cpp match foo_exp.cpp

#######################################################

# verbosity flag
V?=@

# TESTS contains the list of ml transformation scripts
TESTS=$(wildcard *.ml)

# CHECKS contains the list of targets to be produced
CHECKS=$(FILES:.ml=.chk)

# The build command for compiling a script
BUILD=ocamlbuild -pkgs clangml,refl,pprint,str,optiTrust.scriptTools

all: $(CHECKS)

.PRECIOUS: %.byte %_out.cpp %.chk

#######################################################

# Rule for executing one given test and comparing it to the expected result
%.out: %_out.cpp %_exp.cpp
	$(V)diff -q --ignore-space-change $^ || true
	$(V)cat $<

# Rule for building .chk: compare the output and the expected output
%.chk: %_out.cpp %_exp.cpp 
	$(V)   (diff -q --ignore-space-change $^ > /dev/null && touch $@ && echo "$< matches the expected result") \
	|| (echo "$< does not match the expected result:" && diff --ignore-space-change $^ || true)

# Rule for building the output of a test: build the binary and run it; result depends on input .cpp file
%_out.cpp: %.byte %.cpp
	$(V)./$<

# Rule for building the binary associated with a test
%.byte: %.ml
	$(V)$(BUILD) $@ 


#######################################################
# cleanup

clean:
	$(V)rm -rf *_out.cpp *.byte *.chk
	$(V)rm -rf _build
	echo "Clean successful"

#######################################################

# TODO Begatim: have a vscode shortcurt so that from either the foo.ml or the foo.cpp file,
# typing the shortcut would execute the command
# make foo.out 

# TODO Begatim: it would be nice that the rule for building %.byte has a dependency on
# the opam library ScriptTools, so that after a "make install" we are not forced to
# do a "make clean" by hand in this folder.



