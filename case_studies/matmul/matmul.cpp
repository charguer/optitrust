#include "matmul.h"
#include "../../include/optitrust.h"

/* Multiplies the matrix A (dim m x p) by the matrix B (dim p x n),
 * and writes the result in the matrix C (dim m x n):
 *   C = A * B
 */
void mm(float* C, float* A, float* B, int m, int n, int p) {
// C -> matrix2
// A ->^R matrix2
// B ->^R matrix2
// modifies
//   C -> matrix2
// reads
//   A, B -> matrix2
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      // sum -> cell
      for (int k = 0; k < p; k++) {
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
/*
int main() {
  const int M = 1024;
  const int N = 1024;
  const int P = 1024;
  
  float* C = (float*) calloc(M * N, sizeof(float));
  float* A = (float*) malloc(M * P * sizeof(float));
  float* B = (float*) malloc(P * N * sizeof(float));

  // TODO: init?

  mm(C, A, B, M, N, P);

  // TODO: check result?

  free(C);
  free(A);
  free(B);

  return 0;
}
*/