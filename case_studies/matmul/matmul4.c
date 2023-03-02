#include "../../include/optitrust.h"
#include "matmul.h"
#include "omp.h"

void mm(float* C, float* A, float* B, int m, int n, int p) {
  float* pB = (float*)MALLOC4((n + 31) / 32, (p + 3) / 4, 4, 32, sizeof(float));
#pragma omp parallel for
  for (int bj = 0; bj < n; bj += 32) {
    for (int bk = 0; bk < p; bk += 4) {
      for (int k = 0; k < 4; k++) {
        for (int j = 0; j < 32; j++) {
          pB[j + k * 32 + bk / 4 * 4 * 32 + bj / 32 * ((p + 3) / 4) * 4 * 32] =
              B[j + bj + n * (k + bk)];
        }
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < m; bi += 32) {
    for (int bj = 0; bj < n; bj += 32) {
      float* sum = (float*)MALLOC2(32, 32, sizeof(float));
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[j + i * 32] = 0.;
        }
      }
      for (int bk = 0; bk < p; bk += 4) {
        for (int i = 0; i < 32; i++) {
          for (int j = 0; j < 32; j++) {
            sum[j + i * 32] +=
                A[0 + bk + p * (i + bi)] * pB[j + 0 * 32 + bk / 4 * 4 * 32 +
                                              bj / 32 * ((p + 3) / 4) * 4 * 32];
          }
          for (int j = 0; j < 32; j++) {
            sum[j + i * 32] +=
                A[1 + bk + p * (i + bi)] * pB[j + 1 * 32 + bk / 4 * 4 * 32 +
                                              bj / 32 * ((p + 3) / 4) * 4 * 32];
          }
          for (int j = 0; j < 32; j++) {
            sum[j + i * 32] +=
                A[2 + bk + p * (i + bi)] * pB[j + 2 * 32 + bk / 4 * 4 * 32 +
                                              bj / 32 * ((p + 3) / 4) * 4 * 32];
          }
          for (int j = 0; j < 32; j++) {
            sum[j + i * 32] +=
                A[3 + bk + p * (i + bi)] * pB[j + 3 * 32 + bk / 4 * 4 * 32 +
                                              bj / 32 * ((p + 3) / 4) * 4 * 32];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[j + bj + n * (i + bi)] = sum[j + i * 32];
        }
      }
      free(sum); // ADDED
    }
  }
  free(pB); // ADDED
}
