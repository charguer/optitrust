#include <optitrust.h>

#include "omp.h"
// NOTE: using pretty matrix notation

void mm1024(float* C, float* A, float* B) {
  float* const pB = (float*)malloc(1048576 * sizeof(float));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    for (int bk = 0; bk < 256; bk++) {
      for (int k = 0; k < 4; k++) {
        for (int j = 0; j < 32; j++) {
          pB[32768 * bj + 128 * bk + 32 * k + j] =
              B[32 * bj + 4096 * bk + 1024 * k + j];
        }
      }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    for (int bj = 0; bj < 32; bj++) {
      float* const sum = (float*)malloc(1024 * sizeof(float));
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[32 * i + j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
          float s[32];
          MATRIX1_COPY_float(s, &sum[32 * i], 32);
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[32768 * bi + 4 * bk + 1024 * i] *
                    pB[32768 * bj + 128 * bk + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[32768 * bi + 4 * bk + 1024 * i + 1] *
                    pB[32768 * bj + 128 * bk + j + 32];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[32768 * bi + 4 * bk + 1024 * i + 2] *
                    pB[32768 * bj + 128 * bk + j + 64];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[32768 * bi + 4 * bk + 1024 * i + 3] *
                    pB[32768 * bj + 128 * bk + j + 96];
          }
          MATRIX1_COPY_float(&sum[32 * i], s, 32);
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[32768 * bi + 32 * bj + 1024 * i + j] = sum[32 * i + j];
        }
      }
      free(sum);
    }
  }
  free(pB);
}
