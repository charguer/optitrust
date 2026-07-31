#include <optitrust_models.h>





const int bm = 32;

const int bn = 32;

const int bk = 4;

const int tn = 4;

const int tm = 8;

  void mm (float* c, float* a, float* b, int m, int n, int p)  {
  float* const c_gmem = (float*) malloc(m * n * sizeof(float));
  float* const a_gmem = (float*) malloc(m * p * sizeof(float));
   for (int i1 = 0; i1 < m; i1++) {
     for (int i2 = 0; i2 < p; i2++) {
      a_gmem[i1 * p + i2] = a[i1 * p + i2];
    }
  }
  float* const b_gmem = (float*) malloc(n * p * sizeof(float));
   for (int i1 = 0; i1 < p; i1++) {
     for (int i2 = 0; i2 < n; i2++) {
      b_gmem[i1 * n + i2] = b[i1 * n + i2];
    }
  }
  float* const a_smem = (float*) malloc(exact_div(4 * 4 * 8 * m * n, (32 * 32
  )) * sizeof(float));
  float* const b_smem = (float*) malloc(exact_div(8 * 4 * 4 * m * n, (32 * 32
  )) * sizeof(float));
   for (int bi = 0; bi < exact_div(m, 32); bi++) {
     for (int bj = 0; bj < exact_div(n, 32); bj++) {
      float* const sum = (float*) malloc(4 * 8 * 8 * 4 * sizeof(float));
       for (int ti = 0; ti < 4; ti++) {
         for (int tj = 0; tj < 8; tj++) {
           for (int i = 0; i < 8; i++) {
             for (int j = 0; j < 4; j++) {
              sum[ti * 8 * 8 * 4 + tj * 8 * 4 + 4 * i + j] = 0.f;
            }
          }
        }
      }
       for (int bkIdx = 0; bkIdx < exact_div(p, 4); bkIdx++) {
         for (int ti = 0; ti < 4; ti++) {
           for (int k = 0; k < 4; k++) {
             for (int i = 0; i < 8; i++) {
              a_smem[exact_div(bi * n * 4 * 4 * 8, 32) + bj * 4 * 4 * 8 + ti * 4 * 8 + 8 * k + i] = a_gmem[(
                32 * bi + 8 * ti + i) * p + 4 * bkIdx + k];
            }
          }
        }
         for (int tj = 0; tj < 8; tj++) {
           for (int k = 0; k < 4; k++) {
             for (int j = 0; j < 4; j++) {
              b_smem[exact_div(bi * n * 8 * 4 * 4, 32) + bj * 8 * 4 * 4 + tj * 4 * 4 + 4 * k + j] = b_gmem[(
                4 * bkIdx + k) * n + 32 * bj + 4 * tj + j];
            }
          }
        }
         for (int ti = 0; ti < 4; ti++) {
           for (int tj = 0; tj < 8; tj++) {
             for (int k = 0; k < 4; k++) {
              float* const a_regs = (float*) malloc(8 * sizeof(float));
               for (int i = 0; i < 8; i++) {
                a_regs[i] = a_smem[exact_div(bi * n * 4 * 4 * 8, 32) + bj * 4 * 4 * 8 + ti * 4 * 8 + 8 * k + i];
              }
              float* const b_regs = (float*) malloc(4 * sizeof(float));
               for (int j = 0; j < 4; j++) {
                b_regs[j] = b_smem[exact_div(bi * n * 8 * 4 * 4, 32) + bj * 8 * 4 * 4 + tj * 4 * 4 + 4 * k + j];
              }
               for (int i = 0; i < 8; i++) {
                 for (int j = 0; j < 4; j++) {
                  sum[ti * 8 * 8 * 4 + tj * 8 * 4 + 4 * i + j] += a_regs[i] * b_regs[j];
                }
              }
              free(b_regs);
              free(a_regs);
            }
          }
        }
      }
       for (int ti = 0; ti < 4; ti++) {
         for (int tj = 0; tj < 8; tj++) {
           for (int i = 0; i < 8; i++) {
             for (int j = 0; j < 4; j++) {
              c_gmem[(32 * bi + 8 * ti + i) * n + 32 * bj + 4 * tj + j] = sum[ti * 8 * 8 * 4 + tj * 8 * 4 + 4 * i + j];
            }
          }
        }
      }
      free(sum);
    }
  }
  free(b_smem);
  free(a_smem);
  free(b_gmem);
  free(a_gmem);
   for (int i1 = 0; i1 < m; i1++) {
     for (int i2 = 0; i2 < n; i2++) {
      c[i1 * n + i2] = c_gmem[i1 * n + i2];
    }
  }
  free(c_gmem);
}
