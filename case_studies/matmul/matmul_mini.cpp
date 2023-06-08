#include "matmul.h"
#include "../../include/optitrust.h"

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(float* C, float* A, float* B, int m, int n, int p) {

  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}

void mm1024(float* C, float* A, float* B) {
  mm(C, A, B, 1024, 1024, 1024);
}