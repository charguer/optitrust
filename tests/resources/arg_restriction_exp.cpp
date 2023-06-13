#include "../../include/optitrust.h"

int f(int x) {
  __requires("");
  __ensures("eq(_Res, __mul(x, x));");
  __admitted();
  return x * x;
}

int g(int x) {
  __requires("");
  __ensures("");
  __admitted();
  return 2 * x;
}

int modify(int* t) {
  __consumes("t => Cell;");
  __produces("t => Cell;");
  __admitted();
  return 1;
}

void h(int* t) {
  __consumes("t => Cell;");
  __produces("t => Cell;");
  int r = f(3);
  r = *t;
  const int s0 = g(g(3));
  const int s1 = g(f(3));
}
