#include <optitrust.h>

void simple(int n) {
  __pure();

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> Cell");

    int* const m = MALLOC1(int, n);
    x++;
    free(m);
  }
}

void less_simple(int n) {
  __pure();

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> Cell");

    int* const m = MALLOC1(int, n);
    for (int i = 0; i < n; i++) {
      __xwrites("&m[MINDEX1(n,i)] ~> Cell");
      m[MINDEX1(n, i)] = x + i;
      x += m[MINDEX1(n, i)];
    }
    free(m);
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
