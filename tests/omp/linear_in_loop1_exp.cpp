#include <stdio.h>

const int N = 100;

int main() {
  int j;
  float a[N];
  float b[N / 2];
  for (int i = 0; i < N; i++) 
    a[i] = i + 1;
  j = 0;
#pragma omp parallel
#pragma omp for linear(j : 1)
  for (int i = 0; i < N; i += 2) {
    b[j] = a[i] * 2.;
    j++;
  }
  printf("%d %f %f\n", j, b[0], b[j - 1]);
  return 0;
}
