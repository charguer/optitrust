#include <optitrust.h>

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C ~> Matrix2(m, n)");
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
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

void f() {
  __pure();
  int z = 0;
  int y = z;
  int x = 3;
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
      z = i + j;
    }
    z = i;
    x = z;
  }
  z = x;
}
