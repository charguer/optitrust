#include "optitrust.h"
void different_order(float *x, int n1, int n2, int n3) {
  __modifies("for i2 in 0..n2 -> for i3 in 0..n3 -> for i1 in 0..n1 -> "
          "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
  __admitted();
}

void different_order_caller(float *x, int n1, int n2, int n3) {
  __modifies("for i1 in 0..n1 -> for i2 in 0..n2 -> for i3 in 0..n3 -> "
          "&x[MINDEX3(n1,n2,n3,i1,i2,i3)] ~> Cell");
  different_order(x, n1, n2, n3);
}
