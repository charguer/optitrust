#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"

void mm(float* C, float* A, float* B, int m, int n, int p) {
  float* pB =
      (float*)malloc(sizeof(float[exact_div(n, 32)][exact_div(p, 4)][4][32]));
#pragma omp parallel for
  for (int bj = 0; bj < exact_div(n, 32); bj++) {
    for (int bk = 0; bk < exact_div(p, 4); bk++) {
      for (int k = 0; k < 4; k++) {
        
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          pB[j + 32 * k + bk * 4 * 32 + bj * p * 32] =
              B[32 * bj + j + (4 * bk + k) * n];
        }
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < exact_div(m, 32); bi++) {
    for (int bj = 0; bj < exact_div(n, 32); bj++) {
      float* sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          sum[j + 32 * i] = 0.;
        }
      }
      for (int bk = 0; bk < exact_div(p, 4); bk++) {
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + (32 * bi + i) * p] *
                               pB[j + bk * 4 * 32 + bj * p * 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + 1 + (32 * bi + i) * p] *
                               pB[j + 32 + bk * 4 * 32 + bj * p * 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + 2 + (32 * bi + i) * p] *
                               pB[j + 2 * 32 + bk * 4 * 32 + bj * p * 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[j + 32 * i] += A[4 * bk + 3 + (32 * bi + i) * p] *
                               pB[j + 3 * 32 + bk * 4 * 32 + bj * p * 32];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          C[32 * bj + j + (32 * bi + i) * n] = sum[j + 32 * i];
        }
      }
      free(sum);
    }
  }
  free(pB);
}
