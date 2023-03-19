#include "../../include/optitrust.h"
#include "matmul.h"

// NOTE: using pretty matrix notation
#include "omp.h"
#include <assert.h>

void mm(float* C, float* A, float* B, int m, int n, int p) {
  assert((m == 1024) && (n == 1024) && (p == 1024));

  float* pB =
      (float*)malloc(sizeof(float[32][256][4][32]));
#pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    // for (int bk = 0; bk < 256; bk++) {
    //  for (int k = 0; k < 4; k++) {
    for (int k = 0; k < 1024; k++) {
        int c1 = (k * 32) + (bj * 32768);
        int c2 = (bj * 32) + (k * 1024);
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
          sum[(i* 32) + j] = 0.0f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        for (int i = 0; i < 32; i++) {
          int c3 = (bk << 2) + (bi * 32768) + (i * 1024);
          int c4 = (bk << 7) + (bj * 32768);
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            sum[(i * 32) + j] += A[c3 + 0] * pB[c4 + j];
          }
          for (int j = 0; j < 32; j++) {
            sum[(i * 32) + j] += A[c3 + 1] * pB[(1 * 32) + c4 + j];
          }
          for (int j = 0; j < 32; j++) {
            sum[(i * 32) + j] += A[c3 + 2] * pB[(2 * 32) + c4 + j];
          }
          for (int j = 0; j < 32; j++) {
            sum[(i * 32) + j] += A[c3 + 3] * pB[(3 * 32) + c4 + j];
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        for (int j = 0; j < 32; j++) {
          C[(bj * 32) + (bi * 32768) + (i * 1024) + j] = sum[(i * 32) + j];
        }
      }
    }
  }
  free(pB);
}
