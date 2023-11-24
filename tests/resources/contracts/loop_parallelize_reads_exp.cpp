#include <optitrust.h>

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n)");
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  const __ghost_fn __ghost_pair_2 = __ghost_begin(
      ro_fork_group, "H := B ~> Matrix2(p, n), r := range(0, m, 1)");
  const __ghost_fn __ghost_pair_1 = __ghost_begin(
      ro_fork_group, "H := A ~> Matrix2(m, p), r := range(0, m, 1)");
  for (int i = 0; i < m; i++) {
    __sequentially_modifies("C ~> Matrix2(m, n)");
    __reads("A ~> Matrix2(m, p)");
    __reads("B ~> Matrix2(p, n)");
    const __ghost_fn __ghost_pair_4 = __ghost_begin(
        ro_fork_group, "H := B ~> Matrix2(p, n), r := range(0, n, 1)");
    const __ghost_fn __ghost_pair_3 = __ghost_begin(
        ro_fork_group, "H := A ~> Matrix2(m, p), r := range(0, n, 1)");
    for (int j = 0; j < n; j++) {
      __sequentially_modifies("C ~> Matrix2(m, n)");
      __reads("A ~> Matrix2(m, p)");
      __reads("B ~> Matrix2(p, n)");
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
        __ghost_end(focusA);
        __ghost_end(focusB);
      }
      const __ghost_fn focusC =
          __ghost_begin(matrix2_focus, "M := C, i := i, j := j");
      C[MINDEX2(m, n, i, j)] = sum;
      __ghost_end(focusC);
    }
    __ghost_end(__ghost_pair_3);
    __ghost_end(__ghost_pair_4);
  }
  __ghost_end(__ghost_pair_1);
  __ghost_end(__ghost_pair_2);
}
