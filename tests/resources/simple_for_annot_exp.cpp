#include "../../include/optitrust.h"

void matmul(float* C, float* A, float* B, int m, int n, int p) {
  __modifies("C => Matrix2(m, n);");
  __reads("B => Matrix2(p, n); A => Matrix2(m, p);");
  for (int i = 0; i < m; i++) {
    __sequentially_modifies("C => Matrix2(m, n);");
    __sequentially_reads("B => Matrix2(p, n); A => Matrix2(m, p);");
    for (int j = 0; j < n; j++) {
      float sum = 0.f;
      for (int k = 0; k < p; k++) {
        __sequentially_modifies("sum => Cell;");
        __sequentially_reads("B => Matrix2(p, n); A => Matrix2(m, p);");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
      }
      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
