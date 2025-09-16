#include "optitrust.h"
// void f(float * x, int m, int n ){

//  x[MINDEX2(m,n,1,2)] = 4;
// }
// void g (float *x, int l , int m , int n) {
//   x[MINDEX3(l,m,n,4,1,2)] = 4;
// }

void f(float *x, int m, int n) {
  __modifies("&x[MINDEX2(m,n,1,2)] ~> Cell");
  x[MINDEX2(m, n, 1, 2)] = 4.f;
}
void g(float *x, int l, int m, int n) {
  __modifies("&x[MINDEX3(l,m,n,4,1,2)] ~> Cell");

  x[MINDEX3(l, m, n, 4, 1, 2)] = 4.f;
}

void f2(float *x, int m) {
  __modifies("x ~> Matrix1(m)");
  for (int j = 0; j < m; j++) {
    __xmodifies("&x[MINDEX1(m,j)] ~> Cell");
    x[MINDEX1(m, j)] = 0.f;
  }
}
void g2(float *x, int l, int m) {
  __modifies("x ~> Matrix2(l,m)");
  for (int i = 0; i < l; i++) {
    __xmodifies("for j in 0..m -> &x[MINDEX2(l,m,i,j)] ~> Cell");
    for (int j = 0; j < m; j++) {
      __xmodifies("&x[MINDEX2(l,m,i,j)] ~> Cell");
      x[MINDEX2(l, m, i, j)] = 0.f;
    }
  }
}
