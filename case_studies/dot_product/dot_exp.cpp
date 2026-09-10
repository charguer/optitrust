#include <optitrust_models.h>

#include "omp.h"

float dot(float* a, float* b, int n) {
  float s = 0.f;
  float* const t = (float*)malloc(exact_div(n, 32) * sizeof(float));
#pragma omp parallel for
  for (int bi = 0; bi < exact_div(n, 32); bi++) {
    t[bi] = 0;
    for (int i = 0; i < 32; i++) {
      t[bi] = a[32 * bi + i] * b[32 * bi + i] + t[bi];
    }
  }
  for (int bi = 0; bi < exact_div(n, 32); bi++) {
    s = t[bi] + s;
  }
  free(t);
  return s;
}
