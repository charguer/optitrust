MAIN := matmul.ml

default: out
all: trace

include ../CaseStudies.Makefile

# FIXME: avoid this
%.c: %.cpp
	$(V)cp $< $@

_build/%.so: %.c _build
	$(V)$(CC) $(OPT_FLAGS) -fPIC -shared -std=c11 -I ../../include/ $(WARN_FLAGS) $< -o $@

.PHONY: bench bench_%

MATMUL_C := $(wildcard matmul*.c)
MATMUL_CPP := $(wildcard matmul*.cpp)
MATMUL_BENCHS := $(patsubst %.c,bench_%,$(MATMUL_C)) $(patsubst %.cpp,bench_%,$(MATMUL_CPP))
MATMUL_BENCHS := $(filter-out bench_matmul_after,$(MATMUL_BENCHS))
MATMUL_BENCHS := $(filter-out bench_matmul_before,$(MATMUL_BENCHS))

bench: bench_ref $(MATMUL_BENCHS)

bench_demo: bench_ref bench_matmul bench_matmul_out bench_matmul_out_goal bench_matmul_opt_spec_cse_autosimd_ssum_tmp bench_matmul_opt_spec_cse_bits_ssum_tmp

bench_ref:
	$(V)python3 bench.py

bench_%: _build/%.so
	$(V)python3 bench.py $<

# Halide target: x86-64-linux-avx-avx2-fma-sse41
