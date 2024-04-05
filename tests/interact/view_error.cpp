
#include <optitrust.h>

void f() {
  __pure();

  int a = 3;
  int b = 4;
  a += b;
  for (int i = 0; i < 4; i++) {
    __strict();
    __smodifies("&a ~> Cell");
    a++;
  }
}
