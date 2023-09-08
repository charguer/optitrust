#include "../../include/optitrust.h"

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n);");
  __reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
  for (int i = 0; i < m; i++) {
    __sequentially_reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
    __modifies(
        "Group(range(0, n, 1), fun j -> C[MINDEX2(m, n, i, j)] ~> Cell);");
    for (int j = 0; j < n; j++) {
      __sequentially_reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
      __modifies("C[MINDEX2(m, n, i, j)] ~> Cell;");
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
        __sequentially_modifies("sum ~> Cell;");
        __sequentially_reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
        ghost_matrix2_ro_focus(A, i, k);
        ghost_matrix2_ro_focus(B, k, j);
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        ghost_matrix2_ro_unfocus(A);
        ghost_matrix2_ro_unfocus(B);
      }
      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
