#include <optitrust.h>

void swap(int* n, int* m) {
  __modifies("n ~> Cell, m ~> Cell");

  const int k = *m;
  *m = *n;
  *n = k;
}

void array_computation(float* M, int n) {
  __modifies("M ~> Matrix1(n)");

  int acc = 0;
  for (int i = 0; i < n; ++i) {
    __strict();
    __sequentially_modifies("&acc ~> Cell");
    __reads("&M[MINDEX1(n, i)] ~> Cell, &M[MINDEX1(n, i)] ~> Cell");

    acc += M[MINDEX1(n, i)];
  }

  for (int i = 0; i < n; ++i) {
    __strict();
    __parallel_reads("&acc ~> Cell, &acc ~> Cell");
    __modifies("&M[MINDEX1(n, i)] ~> Cell");

    M[MINDEX1(n, i)] = acc;
  }
}
