#include <optitrust.h>

void f1(int* y) {
  __modifies("y ~> Matrix2(4, 4)");

  int x = 0;
  int z = 0;
  for (int a = 0; a < 4; a++) {
    __strict();
    __smodifies("&x ~> Cell, &z ~> Cell");
    __smodifies("for b in 0..4 -> for c in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~> Cell");

    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&x ~> Cell, &z ~> Cell");
      __xmodifies("for c in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~> Cell");

      x++;
      x++;
      for (int c = 0; c < 4; c++) {
        __strict();
        __xmodifies("&y[MINDEX2(4, 4, b, c)] ~> Cell");
        y[MINDEX2(4, 4, b, c)]++;
      }
      z++;
      z++;
    }
  }
}
