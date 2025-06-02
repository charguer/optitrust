#include <optitrust.h>
const int N = 5000;

void polynomial_multiply(double a[N], double b[N]) {
  double *c = MALLOC1(double,2*N-1);
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      c[i+j] += a[i] * b[j];
    }
  }
}
