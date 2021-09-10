#include <math.h>

void nowait_example2(int n, float *a, float *b, float *c, float *y, float *z) {
#pragma omp parallel
  {
#pragma omp for schedule(static) nowait
    for (int i = 0; (i < n); i++)
      c[i] = ((a[i] + b[i]) / 2.);
#pragma omp for schedule(static) nowait
    for (int j = 0; (j < n); j++)
      z[j] = sqrtf(c[j]);
#pragma omp for schedule(static) nowait
    for (int k = 1; (k < n); k++)
      y[k] = (z[(k - 1)] + a[k]);
  }
}