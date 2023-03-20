#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"

void mm1024(float* C, float* A, float* B) {
  float* pB =
      (float*)malloc(sizeof(float[32][256][4][32]));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    // for (int bk = 0; bk < 256; bk++) {
    //  for (int k = 0; k < 4; k++) {
    for (int k = 0; k < 1024; k++) {
        int c1 = (k << 5) + (bj << 15);
        int c2 = (bj << 5) + (k << 10);
        for (int j = 0; j < 32; j++) {
          pB[c1 + j] = B[c2 + j];
        }
    //  }
    }
  }
#pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    for (int bj = 0; bj < 32; bj++) {
      float sum[1024];
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          sum[(i << 5) + j] = 0.0f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
          float tmp[32];
          for (int j = 0; j < 32; j++) {
            tmp[j] = sum[(i << 5) + j];
          }

          int c3 = (bk << 2) + (bi << 15) + (i << 10);
          int c4 = (bk << 7) + (bj << 15);
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[c3 + 0] * pB[c4 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[c3 + 1] * pB[(1 << 5) + c4 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[c3 + 2] * pB[(2 << 5) + c4 + j];
          }
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            tmp[j] += A[c3 + 3] * pB[(3 << 5) + c4 + j];
          }

          for (int j = 0; j < 32; j++) {
            sum[(i << 5) + j] = tmp[j];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[(bj << 5) + (bi << 15) + (i << 10) + j] = sum[(i << 5) + j];
        }
      }
    }
  }
  free(pB);
}
