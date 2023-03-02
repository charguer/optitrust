#include "../../include/optitrust.h"
#include "matmul.h"

// C = A x B
void mm(float* C, float* A, float* B, int m, int n, int p) {
  for (int i = 0; i < m; i++) {
    for (int j = 0; j < n; j++) {
      float sum = 0.;
      for (int k = 0; k < p; k++) {
        float Bt = B[j + n * k];
        sum += A[k + p * i] * Bt;
      }
      C[j + n * i] = sum;
    }
  }
}
