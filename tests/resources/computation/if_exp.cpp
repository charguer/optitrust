#include <optitrust.h>

int f(int x) {
  __pure();
  int r;
  if (x == 0) {
    r = 0;
  } else {
    r = x;
  }
}

void g(int* t, int i) {
  __writes("&t[i] ~> Cell");
  if (i == 0) {
    __ghost(assert_alias, "x := i, y := 0");
    t[0] = 0;
  } else {
    t[i] = 0;
  }
}

void h(int* t, int n) {
  __writes("t ~> Matrix1(n)");
  if (n == 0) {
    __ghost(assert_alias, "x := n, y := 0");
    for (int i = 0; i < 0; i++) {
      __strict();
      __writes("&t[MINDEX1(0, i)] ~> Cell");
      t[MINDEX1(0, i)] = 0;
    }
  } else if (n == 1) {
    __ghost(assert_alias, "x := n, y := 1");
    for (int i = 0; i < 1; i++) {
      __strict();
      __writes("&t[MINDEX1(1, i)] ~> Cell");
      t[MINDEX1(1, i)] = 0;
    }
  } else {
    for (int i = 0; i < n; i++) {
      __strict();
      __writes("&t[MINDEX1(n, i)] ~> Cell");
      t[MINDEX1(n, i)] = 0;
    }
  }
}
