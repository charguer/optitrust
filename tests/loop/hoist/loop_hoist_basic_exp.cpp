#include "../../../include/optitrust.h"

void f(int* t, int* u) {
  __modifies("Group(range(0, 10, 1), fun i -> &u[i] ~> Cell)");
  __reads("Group(range(0, 10, 1), fun i -> &t[i] ~> Cell)");
  int* const x_step = (int* const)MALLOC1(10, sizeof(int));
  int* const z_step = (int* const)MALLOC1(10, sizeof(int));
  for (int i = 0; i < 10; i++) {
    __modifies(
        "&z_step[MINDEX1(10, i)] ~> Cell, &x_step[MINDEX1(10, i)] ~> Cell, "
        "&u[i] ~> Cell");
    __reads("&t[i] ~> Cell");
    int* const x = &x_step[MINDEX1(10, i)];
    x[MINDEX0()] = t[i];
    u[i] = x[MINDEX0()];
    int* const z = &z_step[MINDEX1(10, i)];
    z[MINDEX0()] = x[MINDEX0()];
    int w = 0;
  }
  MFREE1(10, z_step);
  MFREE1(10, x_step);
  int* const yl = (int* const)MALLOC3(5, 4, 8 / 2, sizeof(int));
  for (int l = 0; l < 5; l++) {
    __ghost(group_focus_subrange,
            "wand_id := 1, start := 2, stop := 6, bound_check_start := "
            "checked, bound_check_stop := checked");
    int* const ym = &yl[MINDEX3(5, 4, 8 / 2, l, 0, 0)];
    for (int m = 2; m < 6; m++) {
      __modifies("&ym[MINDEX2(4, 8 / 2, m - 2, 0)] ~> Cell, &u[m] ~> Cell");
      int* const yn = &ym[MINDEX2(4, 8 / 2, m - 2, 0)];
      for (int n = 4; n < 11; n += 2) {
        int* const y = &yn[MINDEX1(8 / 2, (n - 4) / 2)];
        y[MINDEX0()] = 0;
        u[m] = y[MINDEX0()];
      }
    }
    __ghost(close_wand, "wand_id := 1");
  }
  MFREE3(5, 4, 8 / 2, yl);
}
