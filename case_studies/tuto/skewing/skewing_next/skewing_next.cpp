#include "optitrust.h"

const int N = 5000;

void polynomial_multiply(double a[N], double b[N]) {
 double* c = (double*)malloc(MSIZE1(2 * N - 1) * sizeof(double));
 for (int i = 0; i < N; i++) {
  for (int j = 0; j < N; j++) {
   for (int k = 0; k < 2 * N - 1; k++) {
    if (j == k-i) {
     c[k] += a[i] * b[j];
    }
   }
  }
 }
}
