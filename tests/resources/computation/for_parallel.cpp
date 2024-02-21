#include <optitrust.h>

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");

  for (int i = 0; i < m; i++) {
    __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
    __modifies("Group(range(0, n, 1), fun j -> &C[MINDEX2(m, n, i, j)] ~> Cell)");
    for (int j = 0; j < n; j++) {
      __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
      __modifies("&C[MINDEX2(m, n, i, j)] ~> Cell");
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __parallel_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
        __sequentially_modifies("&sum ~> Cell");
        __GHOST_BEGIN(focusA, matrix2_ro_focus, "A, i, k");
        __GHOST_BEGIN(focusB, matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __GHOST_END(focusB);
        __GHOST_END(focusA);
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
