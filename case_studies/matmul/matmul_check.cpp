#include <optitrust.h>

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __writes("C ~> Matrix2(m, n)");

  for (int i = 0; i < m; i++) {
    __xwrites("for j in 0..n -> &C[MINDEX2(m, n, i, j)] ~> Cell");

    for (int j = 0; j < n; j++) {
      __xwrites("&C[MINDEX2(m, n, i, j)] ~> Cell");

      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __GHOST_BEGIN(focusA, ro_matrix2_focus, "A, i, k");
        __GHOST_BEGIN(focusB, ro_matrix2_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __GHOST_END(focusA);
        __GHOST_END(focusB);
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}

void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __writes("C ~> Matrix2(1024, 1024)");

  mm(C, A, B, 1024, 1024, 1024);
}
