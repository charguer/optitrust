#include "optitrust.h"
// void set_test(float *x, int n1) {
//   __modifies("for i1 in 0..n1 -> &x[MINDEX1(n1,i1)] ~> Cell");
//   x[MINDEX1(n1, 2)] = 3.f;
// }
void test_multi_set(float *x, float *y, int n) {
  __modifies("x ~> Matrix1(n)");

  x[MINDEX1(n, 0)] = x[MINDEX1(n, 1)] + 1.f;
}

void get_caller(float *x, float *y, int n) {
  __reads("x ~> Matrix1(n)");
  __reads("y ~> Matrix1(n)");
  __pure();
}
void multi_get(float *x, int m, int n) {
  __reads("x ~> Matrix2(m,n)");
  get_caller(&x[MINDEX2(m, n, 0, 0)], &x[MINDEX2(m, n, 1, 0)], n);
}
void get_set_test(float *x, float *y, int n) {
  __modifies("x ~> Matrix1(n)");
  __reads("y ~> Matrix1(n)");
  x[MINDEX1(n, 0)] = y[MINDEX1(n, 0)];
}
