# Assume MAIN is the filename of the script

out: $(MAIN:.ml=_out.cpp)
trace: $(MAIN:.ml=_trace.js)

include ../Common.Makefile

.PHONY: _build out trace
_build:
	$(V)mkdir -p _build

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

# FIXME: broken rule
icx_analyze_%: _build/%.so
	$(V)icx $(OPT_FLAGS) --analyze -qopt-report=max -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

WARN_FLAGS := -Wall -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi -fdiagnostics-color

clean::
	$(V)rm -Rf _build
