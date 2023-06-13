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

void incr_both(int* n, int* m) {
  __consumes("m => Cell; n => Cell;");
  __produces("m => Cell; n => Cell;");
  incr(n);
  incr(m);
}

int main() {}
