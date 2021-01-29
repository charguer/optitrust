
// Benchmark parameters
const int NUM_PARTICLES = (1 << 27); // must be a multiple of 1<<10.
const int NB_POPULATE = (NUM_PARTICLES >> 6); // (1 << 21)

// Common includes
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdio.h>

// calloc_aligned combines calloc and alloc_aligned
void* calloc_aligned(size_t alignment, size_t size) {
  void* p = aligned_alloc(alignment, size);
  memset(p, 0, size);
  return p;
}

// If the architecture supports vector size 512 (AVX-512 instructions)
#if defined(__AVX512F__)
const int ALIGNMENT = 64;
// else if it supports vector size 256 (AVX instructions)
#elif defined(__AVX__)
const int ALIGNMENT = 32;
// else probably it supports vector size 128 (SSE instructions).
#else
const int ALIGNMENT = 16;
#endif

// For an allocation to be aligned, use the macro ALIGNED before a type.
#define ALIGNED _Alignas(ALIGNMENT)
// Note: before C11, syntax was "type __attribute__((aligned(ALIGNMENT)))"

// For measuring the execution time
clock_t time_start;

void start_clock() {
  time_start = clock();
}

void stop_clock_and_report() {
  clock_t time_end = clock();
  double cpu_time_used = ((double) (time_end - time_start)) / CLOCKS_PER_SEC;
  printf("%.3f\n", cpu_time_used);
}
