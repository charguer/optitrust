#include "../../include/optitrust.h"

int f(int x) {
    __ensures("eq(_Res, __mul(x, x));");
    __admitted();
    return x*x;
}

int g(int x) {
    __pure();
    __admitted();
    return 2*x;
}

int modify(int* t) {
    __modifies("t => Cell;");
    __admitted();
    return 1;
}

void h(int* t) {
    __modifies("t => Cell;");
    int r = f(3);
    r = *t;
    const int s0 = g(g(3));
    const int s1 = g(f(3));
    /*const int s2 = f(f(3)); // Should fail*/
    /*r = modify(t); // Should fail*/
}
