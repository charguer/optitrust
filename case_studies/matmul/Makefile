TARGET_MAKE_ALL := transfo

OPTITRUST := ../..

TESTS := matmul.ml

include ../../tests/Common.Makefile

_build:
	$(V)mkdir _build

# FIXME: avoid this
%.c: %.cpp
	$(V)cp $< $@

# -march=core-avx2 -mtune=core-avx2
# -march=native -mtune=native
CC := clang
OPT_FLAGS := -march=native -mtune=native -Ofast -ffast-math -fopenmp -fno-tree-vectorize

# CC := icx
# OPT_FLAGS := -xhost -fiopenmp

WARN_FLAGS := -Wall -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi

_build/%.so: %.c _build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -shared -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

.PHONY: bench bench_%

MATMUL_C := $(wildcard matmul*.c)
MATMUL_CPP := $(wildcard matmul*.cpp)
MATMUL_BENCHS := $(patsubst %.c,bench_%,$(MATMUL_C)) $(patsubst %.cpp,bench_%,$(MATMUL_CPP))
MATMUL_BENCHS := $(filter-out bench_matmul_after,$(MATMUL_BENCHS))
MATMUL_BENCHS := $(filter-out bench_matmul_before,$(MATMUL_BENCHS))

bench: bench_ref $(MATMUL_BENCHS)

bench_demo: bench_ref bench_matmul bench_matmul_opt

bench_ref:
	$(V)python3 bench.py

bench_opt:

bench_%: _build/%.so
	$(V)python3 bench.py $<