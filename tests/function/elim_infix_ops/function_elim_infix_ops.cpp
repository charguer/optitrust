#include <optitrust.h>

void f(int a, int b, int* r) {
  __writes("r ~> Cell");
  *r = a + b;
  // TODO: return a + b;
}


void g() {
  __pure();
  int a = 3;
  a++;
  a += 1;
  a *= 2;
  a -= 3;
}

void h() {
  __pure();
  int b = 3;
  for (int i = 0; i < 2; i++) {
    b++;
    b += 1;
    b *= 2;
    b -= 3;
  }
}

