#include "optitrust.h"
void ghost_pairs(int *x) {
  __reads("x ~> Matrix1(1)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __sreads("x ~> Matrix1(1)");
    __GHOST_BEGIN(focus_x, ro_matrix1_focus, "x, 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __sreads("&x[MINDEX1(1,0)] ~> Cell");
      x[MINDEX1(1, 0)] + 1;
    }
    __GHOST_END(focus_x);
  }
}

void ghost_pairs2(int *x) {
  __reads("x ~> Matrix1(1)");

  for (int j = 0; j < 5; j++) {
    __sreads("x ~> Matrix1(1)");
    for (int i = 0; i < 5; i++) {
      __GHOST_BEGIN(focus_x, ro_matrix1_focus, "x, 0");

      x[MINDEX1(1, 0)] + 1;
      __GHOST_END(focus_x);
    }
  }
}
