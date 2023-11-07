#include "../../../include/optitrust.h"

void incr(int* a) {
    __admitted();
    *a = *a + 1;
}

void incr_twice(int* a) {
    incr(a);
    incr(a);
}

void incr_both(int* n, int* m) {
    incr(n);
    incr(m);
}

void incr_range(int* m, int sz) {
    for (int i = 0; i < sz; ++i) {
        incr(&m[MINDEX1(sz, i)]);
    }
}

void loop(int n) {
    __pure();
    int acc = 0;
    for (int j = 0; j < n; ++j) {
      ++acc;
    }
}
