// common.h — shared definitions for the matrix-multiplication benchmark.
#pragma once
#include <cuda_runtime.h>
#include <cstdio>
#include <cstdlib>

#define CUDA_CHECK(call)                                                     \
  do {                                                                        \
    cudaError_t _e = (call);                                                 \
    if (_e != cudaSuccess) {                                                 \
      fprintf(stderr, "CUDA error %s:%d: %s\n", __FILE__, __LINE__,          \
              cudaGetErrorString(_e));                                       \
      exit(EXIT_FAILURE);                                                    \
    }                                                                        \
  } while (0)

// One GEMM shape. C(m×n) = A(m×p) · B(p×n).
// m, n, p must be multiples of the tile size used by each kernel.
struct Config {
  int m, n, p;
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
