#include <optitrust.h>

void swap(int* n, int* m) {
  __preserves("n ~> Cell");
  __preserves("m ~> Cell");
  const int k = *m;
  *m = *n;
  *n = k;
}

void array_computation(float* M, int n) {
  __preserves("M ~> Matrix1(n)");
  float acc = 0.f;
  for (int i = 0; i < n; ++i) {
    __strict();
    __spreserves("&acc ~> Cell");
    __xreads("&M[MINDEX1(n, i)] ~> Cell");
    __xreads("&M[MINDEX1(n, i)] ~> Cell");
    acc += M[MINDEX1(n, i)];
  }
  for (int i = 0; i < n; ++i) {
    __strict();
    __sreads("&acc ~> Cell");
    __sreads("&acc ~> Cell");
    __xpreserves("&M[MINDEX1(n, i)] ~> Cell");
    M[MINDEX1(n, i)] = acc;
  }
}
