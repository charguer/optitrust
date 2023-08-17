#include "matmul.h"
#include "../../include/optitrust.h"

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
  __modifies("C ~> Matrix2(m, n);");

  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        ghost_matrix2_ro_focus(A, i, k);
        ghost_matrix2_ro_focus(B, k, j);
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        ghost_matrix2_ro_unfocus(A);
        ghost_matrix2_ro_unfocus(B);
      }

      ghost_matrix2_focus(C, i, j);
      C[MINDEX2(m, n, i, j)] = sum;
      ghost_matrix2_unfocus(C);
    }
  }
}

void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024); B ~> Matrix2(1024, 1024);");
  __modifies("C ~> Matrix2(1024, 1024);");

  mm(C, A, B, 1024, 1024, 1024);
}