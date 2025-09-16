#include <optitrust.h>

void g(int x, int y) {
  __pure();
}


void f(int x) {
  __pure();
  int a = x+1;
  g(a, x);
}


void h() {
  __pure();
  int r = 5;
  const int r_pure = r;
  int b = (r_pure+2)+1;
  g(b, r_pure+2);
  int s = r_pure;
}
void mindex_fold(float*x, int n)
{
  __writes("&x[MINDEX1(n,0)] ~> Cell");
  x[MINDEX1(n,0)] = 12.f;

}
void test_mindex_fold(float *x, int m, int n){
  __writes("&x[MINDEX2(m,n,1,0)] ~> Cell");
  x[MINDEX2(m,n,1,0)] = 12.f;
}
