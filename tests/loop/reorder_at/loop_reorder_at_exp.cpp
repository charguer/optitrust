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
            "outer_range := 0..4, inner_range := 0..4, items := fun b, c -> "
            "&y[MINDEX2(4, 4, b, c)] ~> Cell");
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
            "outer_range := 0..4, inner_range := 0..4, items := fun c, b -> "
            "&y[MINDEX2(4, 4, b, c)] ~> Cell");
    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&z ~> Cell");
      z++;
      z++;
    }
  }
}

void f2(float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  float* const sum = (float* const)MALLOC2(m, n, sizeof(float));
  for (int i = 0; i < m; i++) {
    __strict();
    __xwrites("for _v1 in 0..n -> &sum[MINDEX2(m, n, i, _v1)] ~> Cell");
    for (int j = 0; j < n; j++) {
      __strict();
      __xwrites("&sum[MINDEX2(m, n, i, j)] ~> Cell");
      sum[MINDEX2(m, n, i, j)] = 0.f;
    }
  }
  for (int k = 0; k < p; k++) {
    __strict();
    __smodifies("sum ~> Matrix2(m, n)");
    __sreads("A ~> Matrix2(m, p)");
    __sreads("B ~> Matrix2(p, n)");
    for (int i = 0; i < m; i++) {
      __strict();
      __sreads("A ~> Matrix2(m, p)");
      __sreads("B ~> Matrix2(p, n)");
      __xmodifies("for j in 0..n -> &sum[MINDEX2(m, n, i, j)] ~> Cell");
      for (int j = 0; j < n; j++) {
        __strict();
        __sreads("A ~> Matrix2(m, p)");
        __sreads("B ~> Matrix2(p, n)");
        __xmodifies("&sum[MINDEX2(m, n, i, j)] ~> Cell");
        __ghost(matrix2_ro_focus, "M := A, i := i, j := k");
        __ghost(matrix2_ro_focus, "M := B, i := k, j := j");
        sum[MINDEX2(m, n, i, j)] +=
            A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "M := A");
        __ghost(matrix2_ro_unfocus, "M := B");
      }
    }
  }
  for (int i = 0; i < m; i++) {
    __strict();
    __xconsumes("for j in 0..n -> &sum[MINDEX2(m, n, i, j)] ~> Cell");
    __xproduces(
        "_Uninit(for _v1 in 0..n -> &sum[MINDEX2(m, n, i, _v1)] ~> Cell)");
    for (int j = 0; j < n; j++) {
      __strict();
      __xconsumes("&sum[MINDEX2(m, n, i, j)] ~> Cell");
      __xproduces("_Uninit(&sum[MINDEX2(m, n, i, j)] ~> Cell)");
      sum[MINDEX2(m, n, i, j)]++;
    }
  }
  MFREE2(m, n, sum);
}

void f1_wrong() {
  __pure();
  int x = 0;
  int y = 0;
  for (int a = 0; a < 4; a++) {
    __strict();
    __smodifies("&x ~> Cell");
    __smodifies("&y ~> Cell");
    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&x ~> Cell");
      __smodifies("&y ~> Cell");
      x = 0;
      for (int c = 0; c < 4; c++) {
        __strict();
        __smodifies("&x ~> Cell");
        x += c;
      }
      y += x;
    }
  }
}
