#include <optitrust.h>

void f(int* t, int* u) {
  __writes("u ~> Matrix1(10)");
  __reads("t ~> Matrix1(10)");
  int* const t2 = (int* const)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites("&t2[MINDEX1(10, i)] ~> Cell");
    __xreads("&t[MINDEX1(10, i)] ~> Cell");
    t2[MINDEX1(10, i)] = t[MINDEX1(10, i)];
  }
  for (int i = 0; i < 10; i++) {
    __strict();
    __xwrites("&u[MINDEX1(10, i)] ~> Cell");
    __xreads("&t2[MINDEX1(10, i)] ~> Cell");
    int x = t2[MINDEX1(10, i)];
    u[MINDEX1(10, i)] = x;
    int z = x;
  }
  MFREE1(10, t2);
  __ghost(matrix1_ro_focus, "M := t, i := 0");
  int* const t02 = (int* const)MALLOC0(sizeof(int));
  t02[MINDEX0()] = t[MINDEX1(10, 0)];
  for (int l = 0; l < 5; l++) {
    __strict();
    __sreads("t02 ~> Matrix0()");
    for (int m = 0; m < 2; m++) {
      __strict();
      __sreads("t02 ~> Matrix0()");
      int x = l + m + t02[MINDEX0()];
    }
  }
  MFREE0(t02);
  __ghost(matrix1_ro_unfocus, "M := t");
  int* const a2 = (int* const)MALLOC1(8, sizeof(int));
  for (int a = 0; a < 8; a++) {
    __strict();
    __xwrites("&a2[MINDEX1(8, a)] ~> Cell");
    a2[MINDEX1(8, a)] = a;
  }
  for (int a = 0; a < 8; a++) {
    __strict();
    __xreads("&a2[MINDEX1(8, a)] ~> Cell");
    int y = 0;
    for (int b = 0; b < 5; b++) {
      __strict();
      __sreads("&a2[MINDEX1(8, a)] ~> Cell");
      for (int c = 0; c < 2; c++) {
        __strict();
        __sreads("&a2[MINDEX1(8, a)] ~> Cell");
        int x = a2[MINDEX1(8, a)] + b + c;
      }
      int z = 0;
    }
  }
  MFREE1(8, a2);
}
