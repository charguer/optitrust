#include "optitrust.h"
float trace(float *x, int n) {
  __reads("for i in 0..n -> &x[MINDEX2(n,n,i,i)] ~> Cell");
  float sum = 0.f;
  for (int i = 0; i < n; i++) {
    sum += x[MINDEX2(n, n, i, i)];
  }
  return sum;
}
int main(float *x, int n) {
  __reads("x ~> Matrix2(n,n)");
  float sum = trace(x, n);
  return 0;
}

#include "optitrust.h"
float trace(float *x, int n) {
  float sum = 0.f;
  for (int i = 0; i < n; i++) {
    sum += x[MINDEX2(n, n, i, i)];
  }
  return sum;
}
int main(float *x, int n) {
  float sum = trace(x, n);
  return 0;
}
