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

void f2(float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __strict();
        __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
        __smodifies("&sum ~> Cell");

        __ghost(matrix2_ro_focus, "A, i, k");
        __ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "A");
        __ghost(matrix2_ro_unfocus, "B");
      }
      sum++;
    }
  }
}

void f1_wrong() {
  __pure();

  int x = 0;
  int y = 0;
  for (int a = 0; a < 4; a++) {
    __strict();
    __smodifies("&x ~> Cell, &y ~> Cell");

    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&x ~> Cell, &y ~> Cell");

      x = 0;
      for (int c = 0; c < 4; c++) {
        x += c;
      }
      y += x;
    }
  }
}
