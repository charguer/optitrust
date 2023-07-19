#include "../../include/optitrust.h"

void incr(int* a) {
  __modifies("a ~> Cell;");
  __admitted();
  *a = *a + 1;
}

void incr_twice(int* k) {
  __modifies("k ~> Cell;");
  incr(k);
  incr(k);
}

void mut_var(int x) {
  __pure();
  const int y = 3;
  int z = y;
  int t = z;
  z = x;
  z += x;
}
