#include "../../../include/optitrust.h"

void f1() {
  __pure();

  int x = 0;
  int y = 0;
  int z = 0;
  for (int a = 0; a < 4; a++) {
    __sequentially_modifies("x ~> Cell, y ~> Cell, z ~> Cell");

    for (int b = 0; b < 4; b++) {
      __modifies("x ~> Cell, y ~> Cell, z ~> Cell");

      x++;
      x++;
      for (int c = 0; c < 4; c++) {
        __modifies("y ~> Cell");
        y++;
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
    __modifies("x ~> Cell, y ~> Cell");

    for (int b = 0; b < 4; b++) {
      __modifies("x ~> Cell, y ~> Cell");

      x = 0;
      for (int c = 0; c < 4; c++) {
        x += c;
      }
      y += x;
    }
  }
}
