#include <optitrust.h>

int f(int x) {
  __ensures("_Res = x * x");
  __admitted();
  return x*x;
}

int g(int x) {
  __pure();
  __admitted();
  return 2*x;
}

void h(int* t) {
  __reads("t ~> Cell");
  int r = f(3);
  r = *t;
  const int s0 = g(g(3));
  const int s1 = g(f(3));
  /*const int s2 = f(f(3)); // Not supported yet */
}

int incr(int* t) {
  __modifies("t ~> Cell");
  __admitted();
  *t += 1;
  return *t;
}

void write_in_args() {
  __pure();
  int x = 0;
  int y = 0;
  int z;
  // q = s++; // Encoding is wrong for operator ++
  x = incr(&y);
  z = incr(&x) + incr(&y);
  x = incr(&x); // Is this UB in C ? In our encoding set(x, incr(x)), it is clearly OK.
}
