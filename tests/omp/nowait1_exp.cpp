#include <math.h>

void nowait_example(int n, int m, float *a, float *b, float *y, float *z) {
#pragma omp parallel
  {
#pragma omp for nowait
    for (int i = 1; (i < n); i++)
      b[i] = ((a[i] + a[(i - 1)]) / 2.);
#pragma omp for nowait
    for (int j = 0; (j < m); j++)
      y[j] = sqrt(z[j]);
  }
}