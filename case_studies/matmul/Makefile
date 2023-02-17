TARGET_MAKE_ALL := transfo

OPTITRUST := ../..

TESTS := matmul.ml

include ../../tests/Common.Makefile

_build:
	$(V)mkdir _build

# FIXME: avoid this
%.c: %.cpp
	$(V)cp $< $@

_build/%.so: %.c _build
	$(V)clang -Ofast -march=native -fPIC -shared -fno-tree-vectorize -std=c11 -I ../../include/ -Wall -Wno-unused-function -Wcast-qual -Wignored-qualifiers -Wno-comment -Wsign-compare -Wno-psabi -fopenmp -Wall $< -o $@

.PHONY: bench bench_ref bench_%

bench: bench_ref bench_matmul bench_matmul_out bench_matmul_rise

bench_ref:
	$(V)python3 bench.py

bench_%: _build/%.so
	$(V)python3 bench.py $<