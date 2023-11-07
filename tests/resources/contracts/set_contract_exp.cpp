#include "../../../include/optitrust.h"

void incr(int* a) {
  __modifies("a ~> Cell");
  __admitted();
  *a = *a + 1;
}

void incr_twice(int* a) {
  __modifies("a ~> Cell");
  incr(a);
  incr(a);
}

void incr_both(int* n, int* m) {
  __modifies("n ~> Cell");
  __modifies("m ~> Cell");
  incr(n);
  incr(m);
}

void incr_range(int* m, int sz) {
  __modifies("m ~> Matrix1(sz)");
  for (int i = 0; i < sz; ++i) {
    __modifies("&m[MINDEX1(sz, i)] ~> Cell");
    incr(&m[MINDEX1(sz, i)]);
  }
}

void loop(int n) {
  __pure();
  int acc = 0;
  for (int j = 0; j < n; ++j) {
    __sequentially_modifies("&acc ~> Cell");
    ++acc;
  }
}
