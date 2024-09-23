#include <optitrust.h>

void g(int x, int y) { __pure(); }

void f(int x) {
  __pure();
  int a = x + 1;
  g(a, x);
}

void h() {
  __pure();
  int r = 5;
  const int r_pure = r;
  f(r_pure + 2);
  int s = r_pure;
}
