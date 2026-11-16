# mm-bench Makefile
#   make            -> build
#   make run        -> full benchmark into ./results
#   make check      -> quick smoke test
#   make analyze    -> post-process results/
#   make clean
#
# RTX PRO 6000 Blackwell (GB202, CC 12.0): sm_120
CUDA_HOME ?= /usr/local/cuda
NVCC      ?= $(CUDA_HOME)/bin/nvcc
GPU_ARCH  ?= sm_120

NVCCFLAGS := -O3 -std=c++17 -arch=$(GPU_ARCH) -lcublas \
             -lineinfo -Xptxas -O3 -I.

BIN  := mm_bench
OUT  := results

.PHONY: all check run analyze clean

all: $(BIN)

$(BIN): benchmark.cu kernels.cuh common.h
	$(NVCC) $(NVCCFLAGS) -o $@ benchmark.cu

check: $(BIN)
	./$(BIN) --iters 5 --draws 1 --out $(OUT) --sizes '128x128x128'

run: $(BIN)
	./$(BIN) --out $(OUT)

analyze:
	python3 analyze.py --dir $(OUT)

clean:
	rm -f $(BIN)
