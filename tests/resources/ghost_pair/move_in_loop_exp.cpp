#include "optitrust.h"

void simple_ghost_in(float* x) {
  __reads("x ~> Matrix1(10)");
  for (int i = 0; i < 10; i++) {
    __requires("_Fraction");
    const __ghost_fn focus =
        __ghost_begin(ro_matrix1_focus, "i := 1, n := 10, matrix := x");
    float a = x[MINDEX1(10, 1)];
    __ghost_end(focus);
  }
}

void delete_sreads(float* x) {
  __reads("x ~> Matrix2(2, 1)");
  for (int j = 0; j < 10; j++) {
    __strict();
    __sreads("x ~> Matrix2(2, 1)");
    for (int i = 0; i < 2; i++) {
      __requires("_Fraction");
      const __ghost_fn __ghost_pair_1 =
          __ghost_begin(ro_group_focus,
                        "range := 0..2, i := 1, items := fun (l: int) -> for k "
                        "in 0..1 -> &x[MINDEX2(2, 1, l, k)] ~> Cell");
      __ghost([&]() {
        __requires("#_5: _Fraction");
        __consumes(
            "_RO(#_5, for k in 0..1 -> &x[MINDEX2(2, 1, 1, k)] ~> Cell)");
        __produces("&x[MINDEX2(2, 1, 1, 0)] ~> Cell");
        __produces("&x[MINDEX2(2, 1, 1, 1)] ~> Cell");
        __admitted();
      });
      float a = x[MINDEX2(2, 1, 1, 0)] + x[MINDEX2(2, 1, 1, 1)];
      __ghost([&]() {
        __requires("#_5: _Fraction");
        __consumes("&x[MINDEX2(2, 1, 1, 0)] ~> Cell");
        __consumes("&x[MINDEX2(2, 1, 1, 1)] ~> Cell");
        __produces(
            "_RO(#_5, for k in 0..1 -> &x[MINDEX2(2, 1, 1, k)] ~> Cell)");
        __admitted();
      });
      __ghost_end(__ghost_pair_1);
    }
  }
}
