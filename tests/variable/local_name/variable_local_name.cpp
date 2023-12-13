#include <optitrust.h>

typedef int T;

void ok() {
  __pure();

  T a;
  for (int j = 0; j < 10; j++) {
    __sequentially_modifies("a ~> Cell");
    for (int i = 0; i < j; i++) {
      __sequentially_modifies("a ~> Cell");
      a++;
    }
  }

  int y = 0;
}

void ko() {
  __pure();

  T a;
  int& b = a;
  for (int j = 0; j < 10; j++) {
    __sequentially_modifies("a ~> Cell");
    for (int i = 0; i < j; i++) {
      __sequentially_modifies("a ~> Cell");
      a++;
      b++;
    }
  }

  int y = 0;
}
