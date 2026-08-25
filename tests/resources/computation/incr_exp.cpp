#include <optitrust.h>

void incr(int* a) {
  __preserves("a ~> Cell");
  __admitted();
  *a = *a + 1;
}

void incr_twice(int* k) {
  __preserves("k ~> Cell");
  incr(k);
  incr(k);
}

void incr_both(int* n, int* m) {
  __preserves("n ~> Cell");
  __preserves("m ~> Cell");
  incr(n);
  incr(m);
}

int main() {}
