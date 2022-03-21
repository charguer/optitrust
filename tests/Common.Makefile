#######################################################
# Usage:
#    `make`          attempts to build all the *.chk files and compile all code
#    `make transfo`  only execute the transformations
#    `make check`    only produce the checks of the output of the transformations
#    `make compile`  only compiles c code
#    `make foo.cmp`  to check the result of the test `foo` against expected result
#    `make foo.out`  to execute the test `foo` and compare with expected result
#    `make foo.enc`  to execute the test `foo` and output the encoded AST representation
#    `make foo.meld` opens meld to compare the output with the expected output
#    `make V=`       for more verbose output
#    `make VC=`      for more verbose output only for checking C++ compilation
#    `make f`        force rebuilding all (short for "make clean; make")

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

# List of ml files to not include in the list of tests to consider (by default all *.ml are considered)
EXCLUDE_TESTS ?=

# List of ml files to include (by default, all *.ml except those in EXCLUDE_TESTS, and the generated *.ml files)
TESTS ?= $(filter-out $(wildcard *with_lines.ml), $(filter-out $(EXCLUDE_TESTS), $(wildcard *.ml)))

# List of ml files to include in the documentation
TESTS_WITH_DOC ?= $(TESTS)

# List of ml files for which the cpp files should be compiled
COMPILE ?= $(TESTS)

# List of ml files for which the cpp files should be executed for comparison
EXECUTE ?= $(COMPILE)

# Path to the folder containing optitrust main Makefile, on which to call make install
OPTITRUST ?= ../..

# Default target for 'make all'
TARGET_MAKE_ALL ?= check compile

# Path to the OptiTrust installed library
OPTITRUSTLIB ?= $(shell ocamlfind query optitrust)

# Browser for opening documentation
BROWSER ?= chromium-browser

# Flags for executing the programs, coming both from local user flags, and flags from the local Makefile
-include optitrust_flags.sh
FLAGS ?=
FLAGS := $(FLAGS) $(FLAGS_MAKEFILE)

# Choose between native or bytecode compilation ("native" or "byte")
PROGEXT ?= byte

#######################################################
# Targets

# 'make all' runs each of the unit tests (execute the transformation, and compile all c code)
all: $(TARGET_MAKE_ALL)

# 'make f' forces clean before 'make all'
f: clean all

# 'make transfo' executes all the transformations
transfo: $(TESTS:.ml=_out.cpp)

# 'make check' executes all the transformations and check the results against expected results
check: $(TESTS:.ml=.chk)

# 'make exp' adds all the missing expected files
exp: $(TESTS:.ml=.exp)

# 'make reexp' regenerates all the expected files (! USE WITH CAUTION !)
reexp: $(TESTS:.ml=.reexp)
	$(V)echo "Dont forget:  git add *_exp.cpp"

# 'make compile_src' checks that the source files compile
compile_src: $(TESTS:.ml=.prog)

# 'make compile_out' checks that the output files compile
compile_out: $(TESTS:.ml=_out.prog)

# 'make compile' checks that the source and output cpp files both compile
compile: compile_src compile_out

# 'make execute" checks that the source and output cpp file both produce similar output
execute: $(EXECUTE:.ml=.exec)

# 'make optitrust' rebuilds the library, and clean all local files
optitrust: clean
	$(MAKE) -C $(OPTITRUST) install

# 'make recheck' is a shorthand for 'make optitrust' followed with 'make check'
recheck: optitrust check

# 'make expected' produces all the '_exp.cpp' files
expected: $(TESTS:.ml=.exp)


#######################################################
# Rules

# The command for calling diff
DIFF := diff --ignore-blank-lines --ignore-all-space -I '^//'

# The build command for compiling a script
BUILD := ocamlbuild -tag debug -quiet -pkgs clangml,refl,pprint,str,optitrust

# Instruction to keep intermediate files
.PRECIOUS: %.native %.byte %_out.cpp %.chk %_doc.txt %_doc_spec.txt %_doc.js %_doc.html %_doc.cpp %_trace.js %_doc_out.cpp %_with_lines.ml

# Rule for viewing the encoding of an output
%.enc: %_out.cpp
	$(V)cat $*_out_enc.cpp

# Rule for viewing the output of a transformation
%.out: %_out.cpp
	$(V)cat $< 2> /dev/null || echo "Multiple files produced instead of $@."

# Rule for comparing the output with the expected output
# TODO: should use $(DIFF)? TODO: use $* here
%.cmp: %_out.cpp
	$(V)diff -q --ignore-space-change $^ `basename -s _out.cpp $<`_exp.cpp && echo "===> Matches expected output <===" || true
	$(V)cat $<

# Rule for building .chk, that gives evidence whether the output matches the expected output
%.chk: %_out.cpp %_exp.cpp
	$(V) ($(DIFF) -q $^ > /dev/null && touch $@ && echo "Success for $@") \
	|| (echo "=== ERROR: $< does not match the expected result:" && echo "  make $*.meld")
#	|| (echo "$< does not match the expected result:" && $(DIFF) $^)

# Rule for building the output of a test: build the binary and run it; result depends on input .cpp file
#-----begin rules for non-batch mode------
ifeq ($(BATCH),)

# DEPREACTED
# %_out.cpp: %.$(PROGEXT) %.cpp %.ml
#	$(V)OCAMLRUNPARAM=b ./$< $(FLAGS)
#	@echo "Produced $@"

%_out.cpp: %_with_lines.$(PROGEXT) %.cpp %.ml %_with_lines.ml
	$(V)OCAMLRUNPARAM=b ./$< $(FLAGS)
	@echo "Produced $@"

endif
#-----end rules for non-batch mode------

# Rule for building all the steps
%.bigsteps: %_with_lines.$(PROGEXT) %.cpp %.ml %_with_lines.ml
	$(V)rm -rf bigsteps
	$(V)OCAMLRUNPARAM=b ./$< $(FLAGS) -dump-big-steps bigsteps
	@echo "Produced bigsteps/*"

# Rule for building the binary associated with a test
%.$(PROGEXT): %.ml $(OPTITRUSTLIB)
	$(V)$(BUILD) $@

# Rule for building the html file to display the trace associated with a script
# (copy a template, and substitute the JS file name)
%_trace.html: %_trace.js
	$(V)cp $(OPTITRUST)/tools/trace_template.html $@
	$(V)sed -i "s#{TRACEJSFILE}#$<#g;s#{OPTITRUST}#$(OPTITRUST)#g;s#{FILEBASE}#$*#g" $@
	@echo "Produced $@"

# Rule for annotating a transformation script with the line numbers
%_with_lines.ml: %.ml
	$(V)$(OPTITRUST)/.vscode/add_lines.sh $< $@

# Rule for building the js file describing the trace associated with a script
%_trace.js: %_with_lines.byte %.cpp %_with_lines.ml
	$(V)OCAMLRUNPARAM=b ./$< -dump-trace $(FLAGS)

# Rule for producing the expected output file from the result
# TODO: see if we can use $* instead of basename
%.exp: %_out.cpp
	$(V)(ls `basename -s .exp $@`_exp.cpp 2> /dev/null && echo "Skipping $@") \
  || (cp $< `basename -s .exp $@`_exp.cpp && \
      echo "Produced and should git add `basename -s .exp $@`_exp.cpp")

# Rule for producing the expected file after deleting the previous one
%.reexp: %_out.cpp
	@rm $*_exp.cpp && (cp $< `basename -s .reexp $@`_exp.cpp && \
	echo "(Re)Produced `basename -s .reexp $@`_exp.cpp from $<.")

# Rule for checking that a file compiles
%.prog: %.cpp
	$(V)gcc -std=c++11 $< -o $@
	@chmod +x $@
	@echo "Compiled $<"

# Rule for comparing output of runs
%.exec: %.prog %_out.prog
	$(V)bash -c "($(DIFF) -q <(./$*.prog) <(./$*_out.prog) && echo \"Checked $@\") \
                || (./$*.prog; echo \"--\"; ./$*_out.prog)"

# Rule for opening meld to compare the output and the expected output
%.meld: %_out.cpp %_exp.cpp
	meld $^

# LATER: we might want to activate more warnings, e.g.
# MOREWARNINGS=-Wall -Wno-unused-variable -Wunused-but-set-variable


#######################################################
# Batch-Targets

# 'make batch_check' is like 'make check' but it builds a single binary for executing all the tests at once.
# It is equivalent to 'make BATCH=1 recheck'.
# For a subset of the tests, use the syntax 'make BATCH=1 TESTS="foo.ml bar.ml"'.

# 'make batch_recheck' is similar but for 'recheck' instead of 'check'.
# 'make batch' is a shorthand for this.
# 'make batch_doc' is currently just an alias for 'make batch_check', followed by 'make doc'
# 'make batch_redoc' is currently just an alias for 'make batch_recheck', followed by 'make doc'
# LATER: we may want 'make batch_doc' to check only the files that belong to $(TESTS_WITH_DOC), but it's minor gain

batch_check: clean_batch
	$(MAKE) BATCH=1 check

batch_recheck: clean_batch
	$(MAKE) BATCH=1 recheck

batch_doc: clean_batch
	$(MAKE) -i BATCH=1 batch_check doc

batch_redoc: clean_batch
	$(MAKE) -i BATCH=1 batch_recheck doc

batch: batch_recheck

clean_batch:
	$(V)rm -f batch.ml

#-----begin rules for batch mode------

ifeq ($(BATCH), 1)

# Create batch.ml by concatenating all tests files, and fixing each call to 'Run.script_cpp'
batch.ml: $(OPTITRUST)/tests/batch_tests.sh $(TESTS)
	$(V) $^ > $@

# Produce all '_out.cpp' files at once by running 'batch.byte' (obtained by compiling 'batch.ml')
$(TESTS:.ml=_out.cpp): batch.$(PROGEXT) $(TESTS:.ml=.cpp)
	$(V)OCAMLRUNPARAM=b ./$< $(FLAGS)
	@echo "Executed batch.$(PROGEXT) to produce all output files"

endif

#-----end rules for batch mode------


#######################################################
# Documentation

# Current folder, to prefix the function names that appear in the JS files
CURDIR := $(shell basename `pwd`)

# Source files that the documentation depends upon
OPTITRUST_SRC := $(wildcard $(OPTITRUST)/src/*.ml)

# CHECKS contains the list of targets to be produced for the documentation
DOCJS := $(TESTS_WITH_DOC:.ml=_doc.js)

# Generate an OCaml file containing the script executed by the demo
%_doc.txt: %.ml
	$(V)$(OPTITRUST)/doc/extract_demo.sh $<
	@echo Produced $@

# Generate an OCaml file containing the spec of the function associated with the test
%_doc_spec.txt: %_doc.txt $(OPTITRUST_SRC)
	$(V)$(OPTITRUST)/doc/extract_spec_for_demo.sh $< $(OPTITRUST)
	@echo Produced $@

# To produce the demo input and output files, execute the unit test
%_doc.cpp %_doc_out.cpp: %_out.cpp
	@echo Produced $*_doc.cpp
	@echo Produced $*_doc_out.cpp

#%_doc_out.cpp: %_out.cpp
#	@echo Produced $@

# Generate a JS file containing the material to be displayed in the doc:
# including the source code, and the full input/output diff
# (first remove leading white lines in _doc.cpp)
#  LATER:  $(V)(test -f $*_doc.cpp) || (echo "ERROR: no script_doc for $*")
%_doc.js: %_out.cpp %_doc.txt %_doc_spec.txt %_doc.cpp # %_doc_out.cpp
	$(V)sed -i '/./,$$!d' $*_doc.cpp
	# Uncomment the line below to hide the documentation of transformations in the %_doc.js
	echo "" > $*_doc_spec.txt
	@echo "function get_diff_$*() { return window.atob(\"`git diff  --ignore-blank-lines --ignore-all-space --no-index -U100 $*_doc.cpp $*_doc_out.cpp | base64 -w 0`\"); }" > $@
	@echo "function get_src_$*() { return window.atob(\"`cat $*_doc.txt | base64 -w 0`\"); }" >> $@
	@echo "function get_spec_$*() { return window.atob(\"`cat $*_doc_spec.txt | base64 -w 0`\"); }" >> $@
	@echo Produced $@
# DEPRECATED:$(CURDIR)__  -- TODO: move above into a separate script

# Use 'make mytransfo_doc.html' to build an html preview of the documentation on that transformation (in $(CURDIR))
%_doc.html: %_doc.js # %_out.cpp %_doc.txt %_doc_spec.txt
	$(V)$(OPTITRUST)/doc/doc_create.sh $(OPTITRUST) $@ $*
	@echo Produced $@

# To check the documentation associated with the demo in a browser, use 'make mytransfo.doc'
%.doc: %_doc.html
	$(V)$(BROWSER) $<

# To check the documentation associated with the demo in a console, use 'make mytransfo.doct'
%.doct: %_doc.js # %_out.cpp %_doc.txt %_doc_spec.txt
	@echo "Produced $*_doc_spec.txt and $*_doc.{txt,cpp,out_cpp}"
	@echo "---------------------"
	$(V)cat $*_doc_spec.txt
	@echo "---------------------"
	$(V)cat $*_doc.txt
	@echo "---------------------"
	$(V)cat $*_doc.cpp
	@echo "---------------------"
	$(V)cat $*_doc_out.cpp
	@echo "---------------------"
	$(V)git diff  --ignore-blank-lines --ignore-all-space --no-index -U100 $*_doc.cpp $*_doc_out.cpp | tail -n +5

# 'make docs' to build all the auxililary *_doc.html
docs: $(DOCJS)

# 'make doc' to build the documentation for all $(TESTS_WITH_DOC) in doc.html
doc: doc.html
	$(V)$(BROWSER) $<

# Rule for building 'doc.html', unless it's already defined in an ad-hoc way (e.g. tests/combi/Makefile)
ifeq ($(SPECIAL_RULE_FOR_DOC_HTML),)
doc.html: $(DOCJS)
	$(V)$(OPTITRUST)/doc/doc_create.sh $(OPTITRUST) $@ $(TESTS_WITH_DOC:.ml=)
	@echo Produced $@
endif

# 'make redoc' to force rebuilding all the documentation files
redoc: cleandoc doc


#######################################################
# Cleanup

clean_chk::
	$(V)rm -f *.chk

cleandoc::
	$(V)rm -f *_doc.txt *_doc.cpp *_doc_out.cpp *_doc_spec.txt *_doc.js *_doc.html *_out.cpp
	@echo "Clean documentation"

clean:: cleandoc
	$(V)rm -f *.js *_out.cpp *.byte *.native *.chk *.log *.ast *.out *.cmi *.cmx *.prog *_enc.cpp *_diff.js *_before.cpp *_after.cpp *_trace.js *_trace.html *_diff.html *_with_exit.ml *_with_lines.ml *.html *_before_* tmp_*  *_fast.ml *_inter.ml batch.ml *.ser *.i

	$(V)rm -rf _build
	@echo "Clean successful"


