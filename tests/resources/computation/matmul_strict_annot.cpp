#include <optitrust.h>

void matmul_seq(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");
  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
    __smodifies("C ~> Matrix2(m, n)");
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __strict();
        __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
        __smodifies("&sum ~> Cell");

        __GHOST_BEGIN(focusA, ro_matrix2_focus, "A, i, k");
        __GHOST_BEGIN(focusB, ro_matrix2_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __GHOST_END(focusA);
        __GHOST_END(focusB);
      }

      __GHOST_BEGIN(focusC, matrix2_focus, "C, i, j");
      C[MINDEX2(m, n, i, j)] = sum;
      __GHOST_END(focusC);
    }
  }
}

void matmul_par(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");

  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
    __xmodifies("for j in 0..n -> &C[MINDEX2(m, n, i, j)] ~> Cell");
    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
      __xmodifies("&C[MINDEX2(m, n, i, j)] ~> Cell");
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __strict();
        __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
        __smodifies("&sum ~> Cell");
        __GHOST_BEGIN(focusA, ro_matrix2_focus, "A, i, k");
        __GHOST_BEGIN(focusB, ro_matrix2_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __GHOST_END(focusB);
        __GHOST_END(focusA);
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
