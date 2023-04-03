#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"

void mm1024(float* C, float* A, float* B) {
  float* pB = (float*)malloc(
      sizeof(float[32][256][4][32]));
#pragma omp parallel for schedule(dynamic, 1)
//#pragma omp parallel for collapse(2) schedule(dynamic, 8)
  for (int bj = 0; bj < 32; bj++) {
    for (int bk = 0; bk < 256; bk++) {
      for (int k = 0; k < 4; k++) {
        for (int j = 0; j < 32; j++) {
          pB[32768 * bj + 128 * bk + 32 * k + j] =
              B[1024 * (4 * bk + k) + 32 * bj + j];
        }
      }
    }
  }
#pragma omp parallel for schedule(dynamic, 1)
//#pragma omp parallel for collapse(2) schedule(dynamic, 8)
for (int bi = 0; bi < 32; bi++) {
    for (int bj = 0; bj < 32; bj++) {
      float* sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[32 * i + j] = 0.;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
          float s[32];
          memcpy(s, &sum[32 * i], sizeof(float[32]));
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[1024 * (32 * bi + i) + 4 * bk + 0] *
                    pB[32768 * bj + 128 * bk + 32 * 0 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[1024 * (32 * bi + i) + 4 * bk + 1] *
                    pB[32768 * bj + 128 * bk + 32 * 1 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[1024 * (32 * bi + i) + 4 * bk + 2] *
                    pB[32768 * bj + 128 * bk + 32 * 2 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[1024 * (32 * bi + i) + 4 * bk + 3] *
                    pB[32768 * bj + 128 * bk + 32 * 3 + j];
          }
          memcpy(&sum[32 * i], s, sizeof(float[32]));
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[1024 * (32 * bi + i) + 32 * bj + j] = sum[32 * i + j];
        }
      }
      free(sum);
    }
  }
  free(pB);
}
