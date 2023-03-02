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
# CC := clang
# OPT_FLAGS := -march=native -mtune=native -Ofast -ffast-math -fopenmp -fno-tree-vectorize

CC := icx
OPT_FLAGS := -xhost -fiopenmp
WARN_FLAGS := -Wall -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi

# clang
_build/%.so: %.c _build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -shared -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

.PHONY: bench bench_%

bench: bench_ref bench_matmul bench_matmul_out bench_matmul_out_fix bench_matmul_out_fix2 bench_matmul_out_fix3 bench_matmul_out_fix6 bench_matmul_out_fix7 bench_matmul_rise bench_matmul_rise2

bench_demo: bench_ref bench_matmul0 bench_matmul1 bench_matmul2 bench_matmul3 bench_matmul4 bench_matmul5 bench_matmul_rise bench_matmul_rise2 bench_matmul_rise3 bench_matmul_rise4

bench_ref:
	$(V)python3 bench.py

bench_%: _build/%.so
	$(V)python3 bench.py $<