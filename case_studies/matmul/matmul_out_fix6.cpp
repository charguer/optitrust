#include "../../include/optitrust.h"
#include "matmul.h"
#include "omp.h"
#include <assert.h>

void mm(float* C, float* A, float* B, int m, int n, int p) {
  assert((m == 1024) && (n == 1024) && (p == 1024));

  float* Bt = (float*)MALLOC2(1024, 1024, sizeof(float));
#pragma omp parallel for
  for (int bj = 0; bj < 1024; bj += 32) {
    for (int k = 0; k < 1024; k++) {
#pragma omp simd
      for (int j = 0; j < 32; j++) {
        // Bt[(j + bj) * p + k]
        // -->
        // Bt[j + (32 * k) + (p * bj)]
        Bt[j + (32 * k) + (1024 * bj)] = B[j + bj + 1024 * k];
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < 1024; bi += 32) {
    for (int bj = 0; bj < 1024; bj += 32) {
      float sum[32 * 32];
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[i * 32 + j] = 0.;
        }
      }
      // Bt[(j + bj) * p + (k + bk)]
      // -->
      // Bt[j + (32 * (k + bk)) + (p * bj)]
      for (int bk = 0; bk < 1024; bk += 4) {
        for (int i = 0; i < 32; i++) {        
          float v[32];
          for (int j = 0; j < 32; j++) {
            v[j] = sum[i * 32 + j];
          }

#pragma omp simd
          for (int j = 0; j < 32; j++) {
            v[j] +=
                A[0 + bk + 1024 * (i + bi)] * Bt[j + (32 * (0 + bk)) + (1024 * bj)];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            v[j] +=
                A[1 + bk + 1024 * (i + bi)] * Bt[j + (32 * (1 + bk)) + (1024 * bj)];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            v[j] +=
                A[2 + bk + 1024 * (i + bi)] * Bt[j + (32 * (2 + bk)) + (1024 * bj)];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            v[j] +=
                A[3 + bk + 1024 * (i + bi)] * Bt[j + (32 * (3 + bk)) + (1024 * bj)];
          }
          
          for (int j = 0; j < 32; j++) {
            sum[i * 32 + j] = v[j];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[j + bj + 1024 * (i + bi)] = sum[i * 32 + j];
        }
      }
    }
  }
  free(Bt); // <
}
