#include <optitrust.h>

void f(int n, int* t, int* c) {
  __requires("H: Hprop");
  __modifies("t ~> Matrix2(3, 4)");
  __modifies("c ~> Cell");
  __modifies("H");
  t[MINDEX2(3, 4, *c, 1)]++;
}
