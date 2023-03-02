#include "../../include/optitrust.h"
#include "matmul.h"

void mm(float* C, float* A, float* B, int m, int n, int p) {
  float* pB = (float*)MALLOC4((n + 31) / 32, (p + 3) / 4, 4, 32, sizeof(float));
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
          for (int k = 0; k < 4; k++) {
            for (int j = 0; j < 32; j++) {
              sum[j + i * 32] += A[k + bk + p * (i + bi)] *
                                 pB[j + k * 32 + bk / 4 * 4 * 32 +
                                    bj / 32 * ((p + 3) / 4) * 4 * 32];
            }
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
