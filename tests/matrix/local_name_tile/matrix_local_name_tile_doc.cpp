#include <optitrust.h>

void f() {
  __pure();

  // TODO: deal with CALLOC
  int* const a = (int* const) MALLOC1(10, sizeof(int));
  for (int i = 3; i < 7; i++) {
    __writes("&a[MINDEX1(10, i)] ~> Cell");
    a[MINDEX1(10, i)] = 0;
  }
  MFREE1(10, a);
}
