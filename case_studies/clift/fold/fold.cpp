#include <optitrust.h>

void sum_1(float *a, int m) {
  __reads("a ~> Matrix1(m)");
  float sum = 0;
  for (int i = 0; i < m; i++) {
      sum += a[MINDEX1(m, i)];
  }
}

void test(float * x, int n, int m) {
  __reads("x ~> Matrix2(n,m)");
  float sum = 0;
  for(int i = 0 ; i < n; i ++ ){
    __xreads("for j in 0..m -> &x[MINDEX2(n,m,i,j)] ~> Cell");
    // __sum_1 requests for k in 0..m &(&x[MINDEX2(n,m,i,0)])[MINDEX1(m,k)] ~> Cell
    // trm_access (x,MINDEX2(n,m,i,j) ==? trm_access (trm_access (x, MINDEX2(n,m,i,0)), MINDEX1(m,k))
    sum_1(&x[MINDEX2(n,m,i,0)], m );
  }
}
