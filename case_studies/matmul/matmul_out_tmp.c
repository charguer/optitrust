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
        for (int j = 0; j < 32; j++) {
          pB[bj * p * 32 + bk * 4 * 32 + 32 * k + j] =
              B[(4 * bk + k) * n + 32 * bj + j];
        }
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < exact_div(m, 32); bi++) {
    for (int bj = 0; bj < exact_div(n, 32); bj++) {
      float* sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[32 * i + j] = 0.;
        }
      }
      for (int bk = 0; bk < exact_div(p, 4); bk++) {
        for (int i = 0; i < 32; i++) {
          float tmp[32];
          for (int j = 0; j < 32; j++) {
            tmp[j] = sum[(i * 32) + j];
          }

#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[(32 * bi + i) * p + 4 * bk + 0] *
                               pB[bj * p * 32 + bk * 4 * 32 + 32 * 0 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[(32 * bi + i) * p + 4 * bk + 1] *
                               pB[bj * p * 32 + bk * 4 * 32 + 32 * 1 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[(32 * bi + i) * p + 4 * bk + 2] *
                               pB[bj * p * 32 + bk * 4 * 32 + 32 * 2 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[(32 * bi + i) * p + 4 * bk + 3] *
                               pB[bj * p * 32 + bk * 4 * 32 + 32 * 3 + j];
          }

          for (int j = 0; j < 32; j++) {
            sum[(i * 32) + j] = tmp[j];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[(32 * bi + i) * n + 32 * bj + j] = sum[32 * i + j];
        }
      }
      free(sum);
    }
  }
  free(pB);
}
