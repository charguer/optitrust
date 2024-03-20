#include <optitrust.h>

int dummy() {
  int x = 5;
  int y = 2;
}

void f() {
  __pure();

  int a = 3;
  int b = 4;
  a += b;
  for (int i = 0; i < 4; i++) {
    __strict();
    __sequentially_modifies("&a ~> Cell");
    a++;
  }
}
