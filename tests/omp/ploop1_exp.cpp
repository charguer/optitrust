#include <omp.h>

void simple(int n, float* a, float* b) {
#pragma omp parallel for
  for (int i = 1; i < n; i++) b[i] = (a[i] + a[i - 1]) / 2.;
}
