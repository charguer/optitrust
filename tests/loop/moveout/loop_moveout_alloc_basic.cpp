#include <optitrust.h>

void alloc(int n) {
  __pure();

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> Cell");

    int* const m = (int* const) MALLOC1(n, sizeof(int));
    x++;
    MFREE1(n, m);
  }
}

void var_wrong(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("_Uninit(&x ~> Cell)");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    x = 3;
    t[MINDEX1(3, i)] = x;
  }
}
