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
              B[1024 * (4 * bk + k) + 32 * bj + j];
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
          float* const s = ref[32] float();
          memcpy(&s[0], &sum[32 * i], 32 * sizeof(float));
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[1024 * (32 * bi + i) + 4 * bk] *
                    pB[32768 * bj + 128 * bk + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[1 + 1024 * (32 * bi + i) + 4 * bk] *
                    pB[32 + 32768 * bj + 128 * bk + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[2 + 1024 * (32 * bi + i) + 4 * bk] *
                    pB[64 + 32768 * bj + 128 * bk + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            s[j] += A[3 + 1024 * (32 * bi + i) + 4 * bk] *
                    pB[96 + 32768 * bj + 128 * bk + j];
          }
          memcpy(&sum[32 * i], &s[0], 32 * sizeof(float));
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
