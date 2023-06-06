#include "../../include/optitrust.h"

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

int main() {}
