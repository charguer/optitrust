#include <optitrust.h>

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");
  for (int i = 0; i < m; i++) {
    __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
    __sequentially_modifies("C ~> Matrix2(m, n)");
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
        __sequentially_modifies("&sum ~> Cell");

        __GHOST_BEGIN(focusA, matrix2_ro_focus, "A, i, k");
        __GHOST_BEGIN(focusB, matrix2_ro_focus, "B, k, j");
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
