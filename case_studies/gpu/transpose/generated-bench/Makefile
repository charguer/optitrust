NVCC ?= nvcc
CXX ?= g++

GPU_ARCH ?= $(shell nvidia-smi --query-gpu=compute_cap --format=csv,noheader 2>/dev/null | head -n1 | tr -d '.' | sed 's/^/sm_/')
ifeq ($(GPU_ARCH),)
GPU_ARCH := sm_80
endif

CXXFLAGS := -std=c++17 -O3 -march=native -Wall -Wextra
NVCCFLAGS := -std=c++17 -O3 -arch=$(GPU_ARCH) -lineinfo \
             -Xcompiler=-O3 -Xcompiler=-march=native -Xcompiler=-Wall
LDFLAGS := -arch=$(GPU_ARCH)

TARGET := transpose_bench
OBJS := main.o transpose_benchmark.o

OUT ?= results
ARGS ?=

all: $(TARGET)

main.o: main.cpp transpose_benchmark.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

transpose_benchmark.o: transpose_benchmark.cu transpose_benchmark.h optitrust_gpu_cuda.cuh stats.h
	$(NVCC) $(NVCCFLAGS) -c $< -o $@

$(TARGET): $(OBJS)
	$(NVCC) $(LDFLAGS) -o $@ $(OBJS)

test: test_stats
	./test_stats

test_stats: tests/test_stats.cpp stats.h
	$(CXX) $(CXXFLAGS) -o $@ tests/test_stats.cpp

run: $(TARGET)
	./$(TARGET) --out $(OUT) $(ARGS)

quick: $(TARGET)
	./$(TARGET) --out $(OUT) --inputs 2 --runs 30 --warmup 5 $(ARGS)

analyze: run
	python3 analyze.py $(OUT)

clean:
	rm -f $(TARGET) $(OBJS) test_stats
	rm -rf results

.PHONY: all run quick analyze clean test
