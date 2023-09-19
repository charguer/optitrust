#include "../../include/optitrust.h"

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");
  for (int i = 0; i < m; i++) {
    __sequentially_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
    __sequentially_modifies("C ~> Matrix2(m, n)");
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __sequentially_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
        __sequentially_modifies("sum ~> Cell");

        __ghost(matrix2_ro_focus, "A, i, k");
        __ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "A");
        __ghost(matrix2_ro_unfocus, "B");
      }

      __ghost(matrix2_focus, "C, i, j");
      C[MINDEX2(m, n, i, j)] = sum;
      __ghost(matrix2_unfocus, "C");
    }
  }
}
