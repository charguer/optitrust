#include <optitrust.h>

void f(int a, int b, int* r) {
  __writes("r ~> Cell");
  *r = a + b;
  // TODO: return a + b;
}

void g() {
  __pure();
  int x = 0;
  f(5,x,&x);
  x = x + 3;
  x = 3 + x;
  x = x - 2;
  int y = 4;
  x = x - 2 + y;
}
