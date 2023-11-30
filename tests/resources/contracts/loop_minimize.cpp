#include <optitrust.h>

void h() {
  __pure();

  int x = 0;
  int y = 0;
  int z = 0;

  for (int i = 0; i < 3; ++i) {
    z = y;
  }
}

void f(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix1(n), M2 ~> Matrix1(n)");

  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __modifies("&M1[MINDEX1(n, i)] ~> Cell, &M2[MINDEX1(n, i)] ~> Cell");

    c += M1[MINDEX1(n, i)];
  }
}

