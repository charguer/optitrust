#include <optitrust.h>

void f(int a, int b, int* r) {
  __writes("r ~> Cell");
  *r = a + b;
}

void g() {
  __pure();
  int a = 3;
  a++;
  a = a + 1;
  a = a * 2;
  a = a - 3;
}

void h() {
  __pure();
  int b = 3;
  for (int i = 0; i < 2; i++) {
    __strict();
    __smodifies("&b ~> Cell");
    b++;
    b = b + 1;
    b = b * 2;
    b = b - 3;
  }
}
