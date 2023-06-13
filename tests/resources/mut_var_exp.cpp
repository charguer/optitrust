#include "../../include/optitrust.h"

void incr(int* a) {
  __consumes("a => Cell;");
  __produces("a => Cell;");
  __admitted();
  *a = *a + 1;
}

void incr_twice(int* k) {
  __consumes("k => Cell;");
  __produces("k => Cell;");
  incr(k);
  incr(k);
}

void mut_var(int x) {
  __requires("");
  __ensures("");
  const int y = 3;
  int z = y;
  int t = z;
  z = x;
  z += x;
}
