#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"
#include "assert.h"

void mm(float* C, float* A, float* B, int m, int n, int p) {
  assert((m == 1024) && (n == 1024) && (p == 1024));

  float* pB =
      (float*)malloc(sizeof(float[32][256][4][32]));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    for (int bk = 0; bk < 256; bk++) {
      for (int k = 0; k < 4; k++) {
        
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          pB[j + 32 * k + bk * 4 * 32 + bj * 1024 * 32] =
              B[32 * bj + j + (4 * bk + k) * 1024];
        }
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    for (int bj = 0; bj < 32; bj++) {
      float* sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          sum[j + 32 * i] = 0.;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + (32 * bi + i) * 1024] *
                               pB[j + bk * 4 * 32 + bj * 1024 * 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + 1 + (32 * bi + i) * 1024] *
                               pB[j + 32 + bk * 4 * 32 + bj * 1024 * 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + 2 + (32 * bi + i) * 1024] *
                               pB[j + 2 * 32 + bk * 4 * 32 + bj * 1024 * 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + 3 + (32 * bi + i) * 1024] *
                               pB[j + 3 * 32 + bk * 4 * 32 + bj * 1024 * 32];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          C[32 * bj + j + (32 * bi + i) * 1024] = sum[j + 32 * i];
        }
      }
      free(sum);
    }
  }
  free(pB);
}
