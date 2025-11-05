
#include <optitrust.h>

void f() {
  __pure();
  int a = 3;
  for (int i = 0; i < 4; i++) {
    __strict();
    // missing: __smodifies("&a ~> Cell");
    a++;
  }
}

