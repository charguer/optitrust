#include "../../include/optitrust.h"

void incr(int* a) {
  __consumes("Cell(a);");
  __produces("Cell(a);");
  __admitted();
  *a = *a + 1;
}

void incr_twice(int* a) {
  __consumes("Cell(a);");
  __produces("Cell(a);");
  incr(a);
  incr(a);
}

void incr_both(int* n, int* m) {
  __consumes("Cell(n); Cell(m);");
  __produces("Cell(n); Cell(m);");
  incr(n);
  incr(m);
}

int main() {}
