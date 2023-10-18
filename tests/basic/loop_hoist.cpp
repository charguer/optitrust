#include "../../include/optitrust.h"

void f(int *t, int *u) {
  __reads("Group(range(0, 10, 1), fun i -> &t[i] ~> Cell)");
  __modifies("Group(range(0, 10, 1), fun i -> &u[i] ~> Cell)");

  for (int i = 0; i < 10; i++) {
    __reads("&t[i] ~> Cell");
    __modifies("&u[i] ~> Cell");

    int x;
    x = t[i];
    u[i] = x;
    int z;
    z = x;
    int w = 0;
  }

  for (int l = 0; l < 5; l++) {
    __GHOST_BEGIN(focus2_6, group_focus_subrange, "start := 2, stop := 6, bound_check_start := checked, bound_check_stop := checked");
    for (int m = 2; m < 6; m++) {
      __modifies("&u[m] ~> Cell");

      for (int n = 4; n < 11; n += 2) {
        int y;
        y = 0;
        u[m] = y;
      }
    }
    __GHOST_END(focus2_6);
  }

  // Question:
  // hoist:
  // - int* y_step = (int*) MALLOC1(2, sizeof(int));
  // - becomes:
  //   - int* y_step_step = (int*) MALLOC2(5, 2, sizeof(int));
  // vs:
  // - int* y_step;
  //   y_step = (int*) MALLOC1(2, sizeof(int));
  // - becomes:
  //   - int* y_step_step = (int**) MALLOC1(5, sizeof(int*));
}
