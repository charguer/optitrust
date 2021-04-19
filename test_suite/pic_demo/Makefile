
# verbosity of C file checking
VC?=

#######################################################
# File lists

# TESTS contains the list of ml transformation scripts
TESTS=pic_demo.ml
#$(filter-out $(EXCLUDE_ML), $(wildcard *.ml))

# CHECKS contains the list of targets to be produced for comparing output with expected output
RESULTS=$(TESTS:.ml=.out)

# The build command for compiling a script
BUILD=ocamlbuild -quiet -pkgs clangml,refl,pprint,str,optiTrust.optitrust

# The command for calling diff
DIFF=diff --ignore-blank-lines --ignore-space-change -I '^//'

# List of cpp files found in the folder, exclude *_enc.cpp, and those from EXCLUDE_CPP
CPP_SOURCES_ALL=pic_demo.cpp
#$(wildcard *.cpp) $(TESTS:.ml=_out.cpp)
CPP_SOURCES=$(filter-out $(EXCLUDE_CPP), $(filter-out %_enc.cpp, $(CPP_SOURCES_ALL)))

#######################################################
# Targets

# 'make all' runs each of the unit tests
all: transfo compile

transfo: $(RESULTS)

# 'make f' forces clean before 'make all'
f: clean all

# make 'compile' checks that the cpp files commited and generated all compile
compile: $(CPP_SOURCES:.cpp=.prog)

update: force
	rm -Rf _build
	$(MAKE) -C ../.. install

force:

#######################################################
# Rules

# Instruction to keep intermediate files 
.PRECIOUS: %.byte %_out.cpp %.chk

# Rule for executing one given test and comparing it to the expected result
%.out: %_out.cpp
	@echo Produced $@

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
	$(VC)@echo "Compiled $<"
endif

%.meld: %.cpp %_out.cpp
	meld $^

# LATER: we might want to activate more warnings, e.g.
# MOREWARNINGS=-Wall -Wno-unused-variable -Wunused-but-set-variable 


#######################################################
# Cleanup

clean:
	$(V)rm -rf *_out.cpp *.byte *.chk *.log *.ast *.out *.prog *_enc.cpp *_diff.js
	$(V)rm -rf _build
	@echo "Clean successful"
