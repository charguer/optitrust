// common.h — shared definitions for the transpose benchmark.
#pragma once
#include <cuda_runtime.h>
#include <cstdio>
#include <cstdlib>

// Propagate a CUDA call failure with file/line context.
#define CUDA_CHECK(call)                                                     \
  do {                                                                        \
    cudaError_t _e = (call);                                                 \
    if (_e != cudaSuccess) {                                                 \
      fprintf(stderr, "CUDA error %s:%d: %s\n", __FILE__, __LINE__,          \
              cudaGetErrorString(_e));                                       \
      exit(EXIT_FAILURE);                                                    \
    }                                                                        \
  } while (0)

// One matrix shape to benchmark. H = rows, W = columns of the input.
struct Config {
  int H;
  int W;
};

// Event-based GPU timer. `end()` returns milliseconds.
struct GpuTimer {
  cudaEvent_t start, stop;
  GpuTimer() { CUDA_CHECK(cudaEventCreate(&start));
               CUDA_CHECK(cudaEventCreate(&stop)); }
  ~GpuTimer() { cudaEventDestroy(start); cudaEventDestroy(stop); }
  void begin() { CUDA_CHECK(cudaEventRecord(start)); }
  float end() {
    CUDA_CHECK(cudaEventRecord(stop));
    CUDA_CHECK(cudaEventSynchronize(stop));
    float ms = 0.f;
    CUDA_CHECK(cudaEventElapsedTime(&ms, start, stop));
    return ms;
  }
};
