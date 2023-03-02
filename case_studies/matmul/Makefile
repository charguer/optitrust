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
OPT_FLAGS := -march=native -mtune=native -Ofast -ffast-math -fopenmp -fno-tree-vectorize
WARN_FLAGS := -Wall -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi

_build/%.so: %.c _build
	$(V)clang $(OPT_FLAGS) -fPIC -shared -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

.PHONY: bench bench_ref bench_%

bench: bench_ref bench_matmul bench_matmul_out bench_matmul_out_fix bench_matmul_out_fix2 bench_matmul_out_fix3 bench_matmul_out_fix6 bench_matmul_out_fix7 bench_matmul_rise bench_matmul_rise2

bench_ref:
	$(V)python3 bench.py

bench_%: _build/%.so
	$(V)python3 bench.py $<