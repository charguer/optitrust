#include <optitrust.h>

void f(int a, int b, int* r) {
  __writes("r ~> Cell");
  *r = a + b;
  // TODO: return a + b;
}


void g() {
  __pure();

  int a = 3;
  a = a + 1;
  a = 3 + a;

  int x = 0;
  f(5,x,&x);
  x = x + 3;
  x = 3 + x;
  x = x - 2;

  int y = 4;

  x = x - 2 + y;
  x = y + x - 4;
  x = y - 4 + x;

  x = y - (- x) - 4; // use += because normalized to x = y + x - 4

  x = x - y; // x -= y  because only one element on RHS, and it's a minus
  x = x - 4; // x -= 4  same but testing the case of a constant
  x = x - 4 - y; // for now we do +=, but maybe one would want x -= y + 4

  x = x * 2;
  x = 3 * x * y;

  // NO CHANGE EXPECTED
  x = y - x - 4; // no change expected: infix cannot be used
  x = 2 * x + 4; // no change, in general we do not want x += x + 4

  /* LATER double c = 1.0, d = 1.0;
  c = c + d;
  c = c + 3.4;
  c = c - d;
  c = c - 3.4;
  */
  // LATER
  //
  // c = 4.0 * c / d * 5.0;
  // LATER: x = exact_div(x, 2);
  // LATER x = x * (1/3); // x /= 3
}
