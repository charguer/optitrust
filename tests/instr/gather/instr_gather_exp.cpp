#include <optitrust.h>

void foo() {
  __pure();
  int x = 3;
  int y = 1;
  int z = 5;
  int t = 2;
  for (int i = 0; i < 10; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += 1;
  }
  for (int j = 0; j < 10; j++) {
    __strict();
    __smodifies("&y ~> Cell");
    y += 1;
  }
  for (int k = 0; k < 10; k++) {
    __strict();
    __smodifies("&z ~> Cell");
    z += 1;
  }
}

void with_deps() {
  __pure();
  int a = 0;
  int b = 0;
  int x = 0;
  int y = 0;
  int u = 0;
  int v = 0;

  v += 1;
  y += b;
  a += 1;
  x += a;
  u += x;
  b += 1;
}
