#include <optitrust.h>

void f(int a, int b, int* r) {
  __writes("r ~> Cell");
  *r = a + b;
}

void g() {
  __pure();
  int a = 3;
  a += 1;
  a += 3;
  int x = 0;
  f(5, x, &x);
  x += 3;
  x += 3;
  x -= 2;
  int y = 4;
  x += -2 + y;
  x += y - 4;
  x += y - 4;
  x += y - 4;
  x -= y;
  x -= 4;
  x += -4 - y;
  x *= 2;
  x *= 3 * y;
  x = y - x - 4;
  x = 2 * x + 4;
}
