#include <optitrust.h>

void h() {
  __pure();
  int x = 0;
  int y = 0;
  int z = 0;
  for (int i = 0; i < 3; ++i) {
    __sequentially_modifies("&z ~> Cell");
    __sequentially_reads("&y ~> Cell");
    z = y;
  }
}

void f(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix1(n)");
  __modifies("M2 ~> Matrix1(n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __reads("&M1[MINDEX1(n, i)] ~> Cell");
    c += M1[MINDEX1(n, i)];
  }
}

void g(int* t2) {
  __consumes("t2 ~> Matrix1(10)");
  __produces("_Uninit(t2 ~> Matrix1(10))");
  for (int i = 0; i < 10; i++) {
    __reads("&t2[MINDEX1(10, i)] ~> Cell");
    int x = t2[MINDEX1(10, i)];
  }
  for (int i = 0; i < 10; i++) {
    __consumes("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");
    __produces("&t2[MINDEX1(10, i)] ~> Cell");
    t2[MINDEX1(10, i)] = 2;
  }
  for (int i = 0; i < 10; i++) {
    __modifies("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");
    t2[MINDEX1(10, i)] = 2;
  }
}
