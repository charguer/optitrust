#include "../../include/optitrust.h"
#include "matmul.h"
#include "omp.h"

void mm(float* C, float* A, float* B, int m, int n, int p) {
  float* Bt = (float*)MALLOC2(n, p, sizeof(float));
#pragma omp parallel for
  for (int bj = 0; bj < n; bj += 32) {
    for (int k = 0; k < p; k++) {
#pragma omp simd
      for (int j = 0; j < 32; j++) {
        // Bt[(j + bj) * p + k]
        // -->
        // Bt[j + (32 * k) + (p * bj)]
        Bt[j + (32 * k) + (p * bj)] = B[j + bj + n * k];
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < m; bi += 32) {
    for (int bj = 0; bj < n; bj += 32) {
      float sum[32 * 32];
      for (int i = 0; i < 32; i++) {
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          sum[i * 32 + j] = 0.;
        }
      }
      // Bt[(j + bj) * p + (k + bk)]
      // -->
      // Bt[j + (32 * (k + bk)) + (p * bj)]
      for (int bk = 0; bk < p; bk += 4) {
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[i * 32 + j] +=
                A[0 + bk + p * (i + bi)] * Bt[j + (32 * (0 + bk)) + (p * bj)];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[i * 32 + j] +=
                A[1 + bk + p * (i + bi)] * Bt[j + (32 * (1 + bk)) + (p * bj)];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[i * 32 + j] +=
                A[2 + bk + p * (i + bi)] * Bt[j + (32 * (2 + bk)) + (p * bj)];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[i * 32 + j] +=
                A[3 + bk + p * (i + bi)] * Bt[j + (32 * (3 + bk)) + (p * bj)];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          C[j + bj + n * (i + bi)] = sum[i * 32 + j];
        }
      }
    }
  }
  free(Bt); // <
}
