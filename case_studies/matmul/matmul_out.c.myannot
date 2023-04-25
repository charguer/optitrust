#include <stdlib.h>

#include "omp.h"

void mm(float* output, float* a, float* b, int m, int n, int o) {
// output -> matrix (m, n) float
// = *(i in [0; m[, j in [0; n[) output[i][j] -> cell float 
// a ->^R matrix (m, o) float
// b ->^R matrix (o, n) float
  float bt[n][o];
  // bt -> matrix (n, o) float
  {
    // matrix_tile_open bt (32, o):
    //   consumes bt -> matrix (n, o) float
    //   produces (*)(bj in 0 to n by 32, x in 0 to o by o)
    //             bt -> matrix_tile ((bj, 32), (x, o)) (n, o) float
    //(unroll x)= (*)(bj in 0 to n by 32)
    //             bt -> matrix_tile ((bj, 32), (0, o)) (n, o) float
#pragma omp parallel for
    for (int bj = 0; bj < n; bj += 32) {
      // only_this_iteration_modifies bt -> matrix_tile ((bj, 32), (0, o)) (n, o) float
      // matrix_tile_open bt (32, 1)
      for (int k = 0; k < o; k++) {
        // only_this_iteration_modifies bt -> matrix_tile ((bj, 32), (k, 1)) (n, o) float
        // matrix_tile_open bt (1, 1)
        //  consumes bt -> matrix_tile ((bj, 32), (k, 1)) (n, o) float
        //  produces (*)(j' in bj to bj+32 by 1, k' in k to k+1 by 1)
        //           bt -> matrix_tile ((j', 1), (k', 1)) (n, o) float
        //           = bt[j' = j + bj][k' = k] -> cell float
#pragma omp simd
        for (int j = 0; j < 32; j++) {
          // bt[j + bj][k] -> cell float
          // =
          // bt -> matrix_tile ((bj+j, 1), (0, o)) (n, o) float
          bt[j + bj][k] = b[j + bj + n * k];
          // i / n ; i % n
          // MINDEX2(j + bj, k)
        }
      }
    }
  }
  {
#pragma omp parallel for
    for (int bi = 0; bi < m; bi += 32) {
      for (int bj = 0; bj < n; bj += 32) {
        // output -> matrix_tile ((bi, 32), (bj, 32)) (m, n) float
        // = *(i in [bi; bi+32[, j in [bj; bj+32[) output[i][j] -> cell float
        float sum[32][32];
        // sum -> matrix (32, 32) float
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            // sum[i][j] -> cell float
            sum[i][j] = 0.;
          }
        }
        for (int bk = 0; bk < o; bk += 4) {
          for (int i = 0; i < 32; i++) {
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              // sum[i][j] -> cell float
              sum[i][j] += a[0 + bk + o * (i + bi)] * bt[j + bj][0 + bk];
            }
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              sum[i][j] += a[1 + bk + o * (i + bi)] * bt[j + bj][1 + bk];
            }
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              sum[i][j] += a[2 + bk + o * (i + bi)] * bt[j + bj][2 + bk];
            }
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              sum[i][j] += a[3 + bk + o * (i + bi)] * bt[j + bj][3 + bk];
            }
          }
        }
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            // output[j + bj + n * (i + bi)] -> cell float
            // =
            // output[i + bi][j + bj] -> cell float
            // =
            // output -> matrix_tile ((bi, 32), (bj+j, 1)) (m, n) float
            output[j + bj + n * (i + bi)] = sum[i][j];
          }
        }
      }
    }
  }
}

int main() {
  const int M = 1024;
  const int N = 1024;
  const int O = 1024;
  float* output = (float*)calloc(M * N, sizeof(float));
  float* a = (float*)malloc(M * O * sizeof(float));
  float* b = (float*)malloc(O * N * sizeof(float));
  mm(output, a, b, M, N, O);
  free(output);
  free(a);
  free(b);
  return 0;
}
