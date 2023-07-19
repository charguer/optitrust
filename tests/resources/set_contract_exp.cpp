#include "../../include/optitrust.h"

void incr(int* a) {
  __modifies("a ~> Cell;");
  __admitted();
  *a = *a + 1;
}

void incr_twice(int* a) {
  __modifies("a ~> Cell;");
  incr(a);
  incr(a);
}

void incr_both(int* n, int* m) {
  __modifies("n ~> Cell; m ~> Cell;");
  incr(n);
  incr(m);
}

int main() {}
