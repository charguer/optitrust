#include <stdio.h>

int const N = 1000000;

void init(int n, float *v1, float *v2);

int main() {
  int i, n = N;
  int chunk = 1000;
  float[N] v1, v2, vxv;
#pragma omp parallel
  init(n, v1, v2);
  {
#pragma omp master
#pragma omp target teams distribute parallel for nowait
    for (int i = 0; (i < (n / 2)); i++) {
      vxv[i] = (v1[i] * v2[i]);
    }
#pragma omp for schedule(dynamic, chunk)
    for (int j = (n / 2); (j < n); j++) {
      vxv[j] = (v1[j] * v2[j]);
    }
  }
  printf(" vxv[0] vxv[n-1] %f %f\n", vxv[0], vxv[(n - 1)]);
  return 0;
}