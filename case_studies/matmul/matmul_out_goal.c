#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"

void mm1024(float* C, float* A, float* B) {
  float* pB = (float*)malloc(
      sizeof(float[32][256][4][32]));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    for (int bk = 0; bk < 256; bk++) {
      for (int k = 0; k < 4; k++) {
        /* for (int j = 0; j < 32; j++) {
          pB[32768 * bj + 128 * bk + 32 * k + j] =
              B[1024 * (4 * bk + k) + 32 * bj + j];
        } */
        memcpy(
          &pB[32768 * bj + 128 * bk + 32 * k],
          &B[1024 * (4 * bk + k) + 32 * bj],
          sizeof(float[32])
        );
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    for (int bj = 0; bj < 32; bj++) {
      // float sum[32 * 32];
      float* sum = (float*)malloc(sizeof(float[32][32]));
      memset(sum, 0, sizeof(float[32][32]));
      /* for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[32 * i + j] = 0.;
        }
      } */
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
          float tmp[32];
          memcpy(tmp, &sum[32 * i], sizeof(float[32]));
          /* for (int j = 0; j < 32; j++) {
            tmp[j] = sum[(i * 32) + j];
          } */
          
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[1024 * (32 * bi + i) + 4 * bk + 0] *
                               pB[32768 * bj + 128 * bk + 32 * 0 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[1024 * (32 * bi + i) + 4 * bk + 1] *
                               pB[32768 * bj + 128 * bk + 32 * 1 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[1024 * (32 * bi + i) + 4 * bk + 2] *
                               pB[32768 * bj + 128 * bk + 32 * 2 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[1024 * (32 * bi + i) + 4 * bk + 3] *
                               pB[32768 * bj + 128 * bk + 32 * 3 + j];
          }

          memcpy(&sum[32 * i], tmp, sizeof(float[32]));
          /* for (int j = 0; j < 32; j++) {
            sum[(i * 32) + j] = tmp[j];
          } */
        }
      }
      for (int i = 0; i < 32; i++) {
        memcpy(
          &C[1024 * (32 * bi + i) + 32 * bj],
          &sum[32 * i], sizeof(float[32])
        );
        /* for (int j = 0; j < 32; j++) {
          C[1024 * (32 * bi + i) + 32 * bj + j] = sum[32 * i + j];
        } */
      }
      free(sum);
    }
  }
  free(pB);
}
