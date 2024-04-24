#include <optitrust.h>

void matmul_seq(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n)");
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  for (int i = 0; i < m; i++) {
    __strict();
    __smodifies("C ~> Matrix2(m, n)");
    __sreads("A ~> Matrix2(m, p)");
    __sreads("B ~> Matrix2(p, n)");
    for (int j = 0; j < n; j++) {
      __strict();
      __smodifies("C ~> Matrix2(m, n)");
      __sreads("A ~> Matrix2(m, p)");
      __sreads("B ~> Matrix2(p, n)");
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
        __strict();
        __smodifies("&sum ~> Cell");
        __sreads("A ~> Matrix2(m, p)");
        __sreads("B ~> Matrix2(p, n)");
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
  }
}

void matmul_par(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n)");
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p)");
    __sreads("B ~> Matrix2(p, n)");
    __xmodifies("for j in 0..n -> &C[MINDEX2(m, n, i, j)] ~> Cell");
    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("A ~> Matrix2(m, p)");
      __sreads("B ~> Matrix2(p, n)");
      __xmodifies("&C[MINDEX2(m, n, i, j)] ~> Cell");
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
        __strict();
        __smodifies("&sum ~> Cell");
        __sreads("A ~> Matrix2(m, p)");
        __sreads("B ~> Matrix2(p, n)");
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
