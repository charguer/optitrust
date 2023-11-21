#include <optitrust.h>

void f1(int* y) {
  __modifies("y ~> Matrix2(4, 4)");
  int x = 0;
  int z = 0;
  for (int a = 0; a < 4; a++) {
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&z ~> Cell");
    __sequentially_modifies(
        "Group(range(0, 4, 1), fun b -> Group(range(0, 4, 1), fun c -> "
        "&y[MINDEX2(4, 4, b, c)] ~> Cell))");
    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&x ~> Cell");
      __modifies(
          "Group(range(0, 4, 1), fun c -> &y[MINDEX2(4, 4, b, c)] ~> Cell)");
      x++;
      x++;
    }
    __ghost(rewrite,
            "H1 := y ~> Matrix2(4, 4), H2 := Group(range(0, 4, 1), fun c -> "
            "Group(range(0, 4, 1), fun b -> &y[MINDEX2(4, 4, b, c)] ~> Cell)), "
            "by := swap_groups");
    for (int c = 0; c < 4; c++) {
      __modifies(
          "Group(range(0, 4, 1), fun b -> &y[MINDEX2(4, 4, b, c)] ~> Cell)");
      for (int b = 0; b < 4; b++) {
        __modifies("&y[MINDEX2(4, 4, b, c)] ~> Cell");
        y[MINDEX2(4, 4, b, c)]++;
      }
    }
    __ghost(rewrite,
            "H1 := Group(range(0, 4, 1), fun c -> Group(range(0, 4, 1), fun b "
            "-> &y[MINDEX2(4, 4, b, c)] ~> Cell)), H2 := y ~> Matrix2(4, 4), "
            "by := swap_groups");
    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&z ~> Cell");
      __modifies(
          "Group(range(0, 4, 1), fun c -> &y[MINDEX2(4, 4, b, c)] ~> Cell)");
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
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&y ~> Cell");
    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&x ~> Cell");
      __sequentially_modifies("&y ~> Cell");
      x = 0;
      for (int c = 0; c < 4; c++) {
        x += c;
      }
      y += x;
    }
  }
}
