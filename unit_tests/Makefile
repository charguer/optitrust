#######################################################
# Usage:
#    `make`          attempts to build all the CHECKS
#    `make foo.out`  to execute the test `foo`
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
# 


############################################# stat##########
# How to create a new test
# 
# # If the transformation is already implemented
#  
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
EXCLUDE_ML= show_path.ml inline_seq.ml fold_ref.ml inline_typ.ml swap_coords_vari.ml  inline_ref.ml fold_typ.ml  inline_fun_vect.ml inline_fun_vect.ml remove_decl.ml ast_encoding.ml inline_struct_in_struct.ml move_loop.ml cstr.ml coloring.ml make_implicit_record_assignment.ml
V?=@

# List of CPP files that are not meant to be compiled
EXCLUDE_CPP=ast_encoding_out.cpp ast_encoding_exp.cpp inline_fun_vect_exp.cpp remove_decl_exp 

# verbosity of C file checking
VC?=no

#######################################################
# File lists

# TESTS contains the list of ml transformation scripts
TESTS=$(filter-out $(EXCLUDE_ML), $(wildcard *.ml))

# CHECKS contains the list of targets to be produced for comparing output with expected output
CHECKS=$(TESTS:.ml=.chk)

# CHECKS contains the list of targets to be produced for the documentation
DIFFJS=$(TESTS:.ml=_diff.js)

# The build command for compiling a script
BUILD=ocamlbuild -quiet -pkgs clangml,refl,pprint,str,optiTrust.optitrust

# The command for calling diff
DIFF=diff --ignore-blank-lines --ignore-space-change -I '^//'

# List of cpp files found in the folder, exclude *_enc.cpp, and those from EXCLUDE_CPP
CPP_SOURCES_ALL=$(wildcard *.cpp) $(TESTS:.ml=_out.cpp)
CPP_SOURCES=$(filter-out $(EXCLUDE_CPP), $(filter-out %_enc.cpp, $(CPP_SOURCES_ALL)))

#######################################################
# Targets

# 'make all' runs each of the unit tests
all: transfo compile

transfo: $(CHECKS)

# 'make f' forces clean before 'make all'
f: clean all

# make 'compile' checks that the cpp files commited and generated all compile
compile: $(CPP_SOURCES:.cpp=.prog)

# 'make doc' to build the auxililary files needed by _doc.html
doc: $(DIFFJS)

# 'make redoc' to force rebuilding all *_diff.js files
redoc:
	rm -f *_diff.js
	$(MAKE) doc

# 'make opendoc' to view the documentation
opendoc: doc
	mkdir -p .chromium
	chromium-browser --new-window --user-data-dir=.chromium --disable-web-security _doc.html

#######################################################
# Rules

# Instruction to keep intermediate files 
.PRECIOUS: %.byte %_out.cpp %.chk

# Rule for executing one given test and comparing it to the expected result
%.out: %_out.cpp
	$(V)diff -q --ignore-space-change $^ `basename -s _out.cpp $<`_exp.cpp && echo "===> Matches expected output <===" || true
	$(V)cat $<

# Rule for building .chk: compare the output and the expected output
%.chk: %_out.cpp %_exp.cpp 
	$(V) ($(DIFF) -q $^ > /dev/null && touch $@ && echo "$< matches the expected result") \
	|| (echo "$< does not match the expected result:" && $(DIFF) $^)

# Rule for producing the diff between the output and the expected output, in a form readable in a browser
%_diff.js: %.cpp %_exp.cpp %.ml
	@echo "function get_diff_$*() { return window.atob(\"`git diff --no-index -U10 $*.cpp $*_exp.cpp | base64 -w 0`\"); }" > $@
	@echo "function get_src_$*() { return window.atob(\"`cat $*.ml | base64 -w 0`\"); }" >> $@
	@echo Produced $@

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

%.meld: %_out.cpp %_exp.cpp
	meld $^

# LATER: we might want to activate more warnings, e.g.
# MOREWARNINGS=-Wall -Wno-unused-variable -Wunused-but-set-variable 


#######################################################
# Cleanup

clean:
	$(V)rm -rf *_out.cpp *.byte *.chk *.log *.ast *.out *.prog *_enc.cpp *_diff.js
	$(V)rm -rf _build
	@echo "Clean successful"

#######################################################

# LATER: it would be nice that the rule for building %.byte has a dependency on
# the opam library Optitrust, so that after a "make install" we are not forced to
# do a "make clean" by hand in this folder.

# TRICK: for viewing the encoding associated with an output file
# NAME=aos_to_soa ; make clean && make ${NAME}.out && echo "--------------" && cat ${NAME}_out_enc.cpp  
#
# to view the encoding associated with an input file, make the .ml file perform no transformation.



