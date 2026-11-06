# transpose-bench Makefile
#   make            -> build
#   make run        -> build + full benchmark into ./results
#   make check      -> quick correctness smoke test
#   make analyze    -> post-process results/
#   make clean
#
# GPU_ARCH must match your device (nvidia-smi / cudaGetDeviceProperties).
# Default is sm_120 for the NVIDIA RTX PRO 6000 Blackwell (GB202, CC 12.0).
# Other Blackwell, CUDA 13.2:
#   make GPU_ARCH=sm_100   (datacenter,          e.g. B100 / B200 / GB200)
#   make GPU_ARCH=sm_120   (consumer/workstation, e.g. RTX 50 / RTX PRO 6000)
#   make GPU_ARCH=sm_121   (GB10,               e.g. DGX Spark / Jetson Thor)
# (Only use the "a" variant, e.g. sm_120a, if you enable arch-specific
#  features such as tcgen05 / TMA. A plain transpose needs no "a" suffix.)
CUDA_HOME ?= /usr/local/cuda
NVCC      ?= $(CUDA_HOME)/bin/nvcc
GPU_ARCH  ?= sm_120

CXXFLAGS  := -O3 -march=native -std=c++17 -Wall -Wextra
NVCCFLAGS := -O3 -std=c++17 -arch=$(GPU_ARCH) \
             -lineinfo -Xptxas -O3 -I.

BIN  := transpose_bench
OUT  := results

.PHONY: all check run analyze clean

all: $(BIN)

$(BIN): benchmark.cu kernels.cuh common.h
	$(NVCC) $(NVCCFLAGS) -o $@ benchmark.cu

check: $(BIN)
	./$(BIN) --iters 5 --draws 1 --out $(OUT) --sizes '512x512,1024x1024'

run: $(BIN)
	./$(BIN) --out $(OUT)

analyze:
	python3 analyze.py --dir $(OUT)

clean:
	rm -f $(BIN)
