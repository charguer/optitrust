#include "optitrust.h"

void simple_focus(float* y, int n) {
 __modifies("&y[MINDEX1(n, 2)] ~> Cell");
 float a = y[MINDEX1(n, 2)];
}

void simple_focus_caller(float* x, int m, int n) {
 __modifies("x ~> Matrix1(n)");
 {
  [&]() {
   __consumes("x ~> Matrix1(n)");
   __produces("&x[MINDEX1(n, 2)] ~> Cell");
   __admitted();
   __with("justif := focus");
  }();
  simple_focus(x, n);
  [&]() {
   __consumes("x ~> Matrix1(n)");
   __produces("&x[MINDEX1(n, 2)] ~> Cell");
   __admitted();
   __with("justif := focus");
  }();
 }
}
