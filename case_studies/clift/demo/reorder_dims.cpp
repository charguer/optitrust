#include "optitrust.h"
void f(int *x, int n, int m) {
  __writes("x ~> Matrix2(n,m)");
  __admitted();
}
void g(int m, int n) {
  __requires("n >0 ");
  __requires("m > 0");
  int *const x = MALLOC2(int, m, n);
  for (int i = 0; i < m; i++) {
    __xmodifies("for j in 0..n -> &x[MINDEX2(m,n,i,j)] ~> UninitCell");
    for (int j = 0; j < n; j++) {
      __xmodifies("&x[MINDEX2(m,n,i,j)] ~> UninitCell");
      x[MINDEX2(m, n, i, j)] = 0;
    }
  }
  free(x);
}
