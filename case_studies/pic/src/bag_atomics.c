
#include "bag_atomics.h"

//==========================================================================
// Auxiliary tool

/* [internal function]
 * Guarantee that the entire value of *p is read atomically.
 * No part of *p can change during the read operation.
 *
 * Taken from openmp-examples-4.5.0.pdf, Example atomic.2.c
 */
chunk* atomic_read_chunk(chunk** p) {
  chunk* v;
  #pragma omp atomic read
  v = *p;
  return v;
}

void atomic_write_chunk(chunk** p, chunk* v) {
  #pragma omp atomic write
  *p = v;
}

int atomic_increment(int* size) {
  int v;
  #pragma omp atomic capture
  v = (*size)++;
  return v;
}
