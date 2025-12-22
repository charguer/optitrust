#include <optitrust.h>

void f1(int* y) {
  __modifies("y ~> Matrix2(4, 4)");
  int x = 0;
  int z = 0;
  for (int a = 0; a < 4; a++) {
    __strict();
    __smodifies("&x ~> Cell");
    __smodifies("&z ~> Cell");
    __smodifies("y ~> Matrix2(4, 4)");
    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&x ~> Cell");
      x++;
      x++;
    }
    __ghost(swap_groups,
            "outer_range := 0..4, inner_range := 0..4, items := fun (b: int) "
            "(c: int) -> &y[MINDEX2(4, 4, b, c)] ~> Cell");
    for (int c = 0; c < 4; c++) {
      __strict();
      __xmodifies("for b in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~> Cell");
      for (int b = 0; b < 4; b++) {
        __strict();
        __xmodifies("&y[MINDEX2(4, 4, b, c)] ~> Cell");
        y[MINDEX2(4, 4, b, c)]++;
      }
    }
    __ghost(swap_groups,
            "outer_range := 0..4, inner_range := 0..4, items := fun (c: int) "
            "(b: int) -> &y[MINDEX2(4, 4, b, c)] ~> Cell");
    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&z ~> Cell");
      z++;
      z++;
    }
  }
}
