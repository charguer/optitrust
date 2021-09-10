#include <stdio.h>

int const N = 100;

void init(int n, float[N] * b);

int main() {
  int j;
  float[N] a, b;
  init(N, b);
  for (int k = 0; (k < N); k++)
    a[k] = 0.;
#pragma omp parallel reduction(+ : a [0:N]) private(j)
  for (int i = 0; (i < N); i++) {
    for (int j = 0; (j < N); j++) {
      a[j] += b[i][j];
    }
  }
  printf(" a[0] a[N-1]: %f %f\n", a[0], a[(N - 1)]);
  return 0;
}