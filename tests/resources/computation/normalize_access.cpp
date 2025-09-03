#include "optitrust.h"
void f0(float * x, int n, int m){
  __modifies(" &(&x[MINDEX2(n,m,1,0)])[MINDEX1(m,1)] ~> Cell");
}
void g0(float * x, int n, int m) {
  __modifies("&x[MINDEX2(n,m,1,1)] ~> Cell");
  f0(x,n,m);
}
void f(float * x, int n, int m){
  __modifies("for i in 0..n  ->  for j in 0..m -> &(&x[MINDEX2(n,m,i,0)])[MINDEX1(m,j)] ~> Cell");
}
void g(float * x, int n, int m) {
  __modifies("x ~> Matrix2(n,m)");
  f(x,n,m);
}

void sum_1(float *a, int m) {
  __reads("a ~> Matrix1(m)");
  // __reads("for i in 0..m -> &a[MINDEX1(m,i)] ~> Cell");
  float sum = 0.f;
  for (int i = 0; i < m; i++) {
    __xreads("&a[MINDEX1(m,i)] ~> Cell");
      sum += a[MINDEX1(m, i)];
  }
}
void test(float * x, int n, int m) {
  __reads("x ~> Matrix2(n,m)");
  float sum = 0.f;
  for(int i = 0 ; i < n; i ++ ){
    __xreads("for j in 0..m -> &x[MINDEX2(n,m,i,j)] ~> Cell");
    // __sum_1 requests for k in 0..m &(&x[MINDEX2(n,m,i,0)])[MINDEX1(m,k)] ~> Cell
    // trm_access (x,MINDEX2(n,m,i,j) ==? trm_access (trm_access (x, MINDEX2(n,m,i,0)), MINDEX1(m,k))
    sum_1(&x[MINDEX2(n,m,i,0)], m );
  }
}
