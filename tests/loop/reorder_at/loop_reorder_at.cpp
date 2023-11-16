#include "../../../include/optitrust.h"

void f1(int* y) {
  __modifies("y ~> Matrix2(4, 4)");

  int x = 0;
  int z = 0;
  for (int a = 0; a < 4; a++) {
    __sequentially_modifies("&x ~> Cell, &z ~> Cell");
    __sequentially_modifies("Group(range(0, 4, 1), fun b -> Group(range(0, 4, 1), fun c -> &y[MINDEX2(4, 4, b, c)] ~> Cell))");

    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&x ~> Cell, &z ~> Cell");
      __modifies("Group(range(0, 4, 1), fun c -> &y[MINDEX2(4, 4, b, c)] ~> Cell)");

      x++;
      x++;
      for (int c = 0; c < 4; c++) {
        __modifies("&y[MINDEX2(4, 4, b, c)] ~> Cell");
        y[MINDEX2(4, 4, b, c)]++;
      }
      z++;
      z++;
    }
  }
}

void f1_wrong() {
  __pure();

  int x = 0;
  int y = 0;
  for (int a = 0; a < 4; a++) {
    __sequentially_modifies("&x ~> Cell, &y ~> Cell");

    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&x ~> Cell, &y ~> Cell");

      x = 0;
      for (int c = 0; c < 4; c++) {
        x += c;
      }
      y += x;
    }
  }
}
