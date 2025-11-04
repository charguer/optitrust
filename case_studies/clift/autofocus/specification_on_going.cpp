#include "optitrust.h"
void ro_modifies_focus(float *x, int n1) {
  __reads("&x[MINDEX1(n1,2)]~>Cell");
  __admitted();
}
void ro_modifies_focus_caller(float *x, int n1) {
  __modifies("for i1 in 0..n1 -> &x[MINDEX1(n1,i1)] ~> Cell");
  ro_modifies_focus(x, n1);
}
