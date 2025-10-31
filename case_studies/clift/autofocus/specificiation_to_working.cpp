#include "optitrust.h"
//  Issue: * is not an unary operator
void indirection(float *x, int *y, int n1, int n2) {
  __reads("&x[MINDEX1(n1,y[MINDEX1(n2,1)])] ~> Cell");
  __admitted();
}
void indirection_caller(float *x, int *y, int n1, int n2) {
  __reads("for i1 in 0..n1 -> &x[MINDEX1(n1,y[MINDEX1(n2,i1)])] ~> Cell");
  indirection(x, y, n1, n2);
}

// Issue: f is not pure in a pure context
void complex_access_generic(float *x, int n1, int n2, int d) {
  __reads(
      "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,(f(i1,i1)),i2)]~> "
      "Cell");

  __admitted();
}

void complex_access_generic_caller(float *x, int n1, int n2) {
  __reads(
      "for i1 in 0..n1 -> for i2 in 0..n2 -> &x[MINDEX2(n1,n2,f(i1,i1),i2)]~>"
      "Cell");
  const int c = 3;
  complex_access_generic(x, n1, n2, c);
}
// Wrong insertion of ghosts, should be on the set and not on the ge
void get_test(float *x ,int n1)
{
  __reads("for i1 in 0..n1 -> &x[MINDEX1(n1,i1)] ~> Cell");
  float a = x[MINDEX1(n1,2)];

}
