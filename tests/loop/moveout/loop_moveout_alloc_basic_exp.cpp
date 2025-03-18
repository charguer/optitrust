#include <optitrust.h>

void simple(int n) {
  __pure();
  int x = 0;
  int* const m = (int*)malloc(MSIZE1(n) * sizeof(int));
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("m ~> UninitMatrix1(n)");
    __smodifies("&x ~> Cell");
    x++;
  }
  free(m);
}

void less_simple(int n) {
  __pure();
  int x = 0;
  int* const m = (int*)malloc(MSIZE1(n) * sizeof(int));
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("m ~> UninitMatrix1(n)");
    __smodifies("&x ~> Cell");
    for (int i = 0; i < n; i++) {
      __strict();
      __smodifies("&x ~> Cell");
      __xwrites("&m[MINDEX1(n, i)] ~> Cell");
      m[MINDEX1(n, i)] = x + i;
      x += m[MINDEX1(n, i)];
    }
  }
  free(m);
}

void var_wrong(int* t) {
  __modifies("t ~> Matrix1(3)");
  int x = 0;
  for (int i = 0; i < 3; i++) {
    __strict();
    __smodifies("&x ~> UninitCell");
    __xmodifies("&t[MINDEX1(3, i)] ~> Cell");
    x = 3;
    t[MINDEX1(3, i)] = x;
  }
}
