# Verbosity for commands
V ?= @

OPTITRUST := ../..
TOOLS_FOLDER := $(OPTITRUST)/tools

.FORCE:

%_with_lines.ml: %.ml
	$(V)$(TOOLS_FOLDER)/add_lines.sh $< $@

%_out.cpp: %_with_lines.ml .FORCE
  @rm -f $@
	$(V)$(TOOLS_FOLDER)/build_cmxs.sh $<
	$(V)OCAMLRUNPARAM=b dune exec optitrust_runner -- $*_with_lines.cmxs $(FLAGS)
	@echo "Produced $@"

  # $(patsubst %.ml,%.cmxs,$<)

# TEST FOR THESE RULES:
# - change foo.ml
# - change foo_with_lines.ml
# - change optitrust lib


# include ../../tests/Common.Makefile

_build:
	$(V)mkdir _build

# -march=core-avx2 -mtune=core-avx2
# -march=native -mtune=native
# CC := clang
# OPT_FLAGS := -march=native -mtune=native -Ofast -ffast-math -fopenmp -fno-tree-vectorize

CC := icx
# -mtune=native -qopt-dynamic-align
# ENABLING QOPT REPORT SEEMS TO IMPROVE PERF => MORE OPTS ENABLED?
OPT_FLAGS := -xhost -fiopenmp -Ofast -vec -qopt-report=max -g
# -vec -fvectorize
# -fsave-optimization-record
# -fmerge-all-constants

icx_analyze_%: _build/%.so
	$(V)icx $(OPT_FLAGS) --analyze -qopt-report=max -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

WARN_FLAGS := -Wall -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi -fdiagnostics-color

clean::
	$(V)rm -f *.js *_out.cpp *.cmxs *.byte *.native *.chk *.log *.ast *.out *.cmi *.cmx *.prog *_enc.cpp *_diff.js *_before.cpp *_after.cpp *_trace.js *_trace.html *_diff.html *_with_exit.ml *_with_lines.ml *.html *_before_* tmp_*  *_fast.ml *_inter.ml batch.ml *.ser *.i *_inlined.cpp
	$(V)rm -Rf _build
	@echo "Clean successful"