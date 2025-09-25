#include "optitrust.h"

void f0(float* x, int n, int m) {
  __modifies("&(&x[MINDEX2(n, m, 1, 0)])[MINDEX1(m, 1)] ~> Cell");
}

void g0(float* x, int n, int m) {
  __modifies("&x[MINDEX2(n, m, 1, 1)] ~> Cell");
  f0(x, n, m);
}

void f(float* x, int n, int m) {
  __modifies("for i in 0..n -> &x[MINDEX2(n, m, i, 0)] ~> Matrix1(m)");
}

void g(float* x, int n, int m) {
  __modifies("x ~> Matrix2(n, m)");
  f(x, n, m);
}

void sum_1(float* a, int m) {
  __reads("a ~> Matrix1(m)");
  float sum = 0.f;
  for (int i = 0; i < m; i++) {
    __strict();
    __smodifies("&sum ~> Cell");
    __xreads("&a[MINDEX1(m, i)] ~> Cell");
    sum += a[MINDEX1(m, i)];
  }
}

void test(float* x, int n, int m) {
  __reads("x ~> Matrix2(n, m)");
  float sum = 0.f;
  for (int i = 0; i < n; i++) {
    __strict();
    __xreads("for j in 0..m -> &x[MINDEX2(n, m, i, j)] ~> Cell");
    sum_1(&x[MINDEX2(n, m, i, 0)], m);
  }
}
