#include <optitrust.h>

void swap(int* n, int* m) {
  __modifies("n ~> Cell");
  __modifies("m ~> Cell");
  const int k = *m;
  *m = *n;
  *n = k;
}

void array_computation(float* M, int n) {
  __modifies("M ~> Matrix1(n)");
  int acc = 0;
  for (int i = 0; i < n; ++i) {
    __strict();
    __smodifies("&acc ~> Cell");
    __xreads("&M[MINDEX1(n, i)] ~> Cell");
    __xreads("&M[MINDEX1(n, i)] ~> Cell");
    acc += M[MINDEX1(n, i)];
  }
  for (int i = 0; i < n; ++i) {
    __strict();
    __sreads("&acc ~> Cell");
    __sreads("&acc ~> Cell");
    __xmodifies("&M[MINDEX1(n, i)] ~> Cell");
    M[MINDEX1(n, i)] = (float)acc;
  }
}
