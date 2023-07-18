#include "../../include/optitrust.h"

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p); B ~> Matrix2(p, n);");
  __modifies("C ~> Matrix2(m, n);");
  for (int i = 0; i < m; i++) {
    //__reads("A; B;");
    //__modifies("C;");
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        //__reads("A; B;")
        //__modifies("sum ~> Cell;")
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
