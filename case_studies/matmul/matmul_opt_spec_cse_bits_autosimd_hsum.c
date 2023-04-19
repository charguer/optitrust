#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"
#include "assert.h"

void mm1024(float* C, float* A, float* B) {
  omp_set_num_threads(4);

  float* pB =
      (float*)malloc(sizeof(float[32][256][4][32]));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    for (int bk = 0; bk < 256; bk++) {
      for (int k = 0; k < 4; k++) {
        int c1 = (k << 5) + (bk << 7) + (bj << 15);
        int c2 = (bj << 5) + (((bk << 2) + k) << 10);
        for (int j = 0; j < 32; j++) {
          pB[j + c1] = B[j + c2];
        }
      }
    }
  }
  float* thread_sums = (float*)malloc(sizeof(float[4][32][32]));
#pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    for (int bj = 0; bj < 32; bj++) {
      float* sum = &thread_sums[omp_get_thread_num() * 32 * 32];
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[j + 32 * i] = 0.;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
          int c3 = (bk << 2) + (((bi << 5) + i) << 10);
          int c4 = (bk << 7) + (bj << 15);
          for (int j = 0; j < 32; j++) {
            sum[j + (i << 5)] += A[c3] * pB[j + c4];
          }
          for (int j = 0; j < 32; j++) {
            sum[j + (i << 5)] += A[1 + c3] * pB[j + (1 << 5) + c4];
          }
          for (int j = 0; j < 32; j++) {
            sum[j + (i << 5)] += A[2 + c3] * pB[j + (2 << 5) + c4];
          }
          for (int j = 0; j < 32; j++) {
            sum[j + (i << 5)] += A[3 + c3] * pB[j + (3 << 5) + c4];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[(bj << 5) + j + (((bi << 5) + i) << 10)] = sum[j + (i << 5)];
        }
      }
    }
  }
  free(thread_sums);
  free(pB);
}
