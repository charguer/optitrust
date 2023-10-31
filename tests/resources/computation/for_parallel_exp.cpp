#include "../../../include/optitrust.h"

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n)");
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  for (int i = 0; i < m; i++) {
    __sequentially_reads("A ~> Matrix2(m, p)");
    __sequentially_reads("B ~> Matrix2(p, n)");
    __modifies(
        "Group(range(0, n, 1), fun j -> &C[MINDEX2(m, n, i, j)] ~> Cell)");
    for (int j = 0; j < n; j++) {
      __sequentially_reads("A ~> Matrix2(m, p)");
      __sequentially_reads("B ~> Matrix2(p, n)");
      __modifies("&C[MINDEX2(m, n, i, j)] ~> Cell");
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
        __sequentially_modifies("&sum ~> Cell");
        __sequentially_reads("A ~> Matrix2(m, p)");
        __sequentially_reads("B ~> Matrix2(p, n)");
        const __ghost_fn focusA =
            __ghost_begin(matrix2_ro_focus, "M := A, i := i, j := k");
        const __ghost_fn focusB =
            __ghost_begin(matrix2_ro_focus, "M := B, i := k, j := j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost_end(focusB);
        __ghost_end(focusA);
      }
      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
