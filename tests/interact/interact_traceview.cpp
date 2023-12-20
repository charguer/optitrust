#include <optitrust.h>

void f() {
  __pure();

  int a = 3;
  int b = 4;
  a += b;
  for (int i = 0; i < 4; i++) {
    __sequentially_modifies("&a ~> Cell");
    a++;
  }
}
