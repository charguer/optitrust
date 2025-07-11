#include <optitrust.h>

void matvec(int col_count, int red_count, float *x, float *y, float *w) {
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] = 0.f;
    for (int k = 0; k < red_count; k++) {
      x[MINDEX1(col_count, j)] +=
          y[MINDEX1(red_count, k)] * w[MINDEX2(col_count, red_count, j, k)];
    }
  }
}
void iter_matvec(int row_count, int col_count, int red_count, float *const x,
                 float *const y, float *const w) {
  for (int i = 0; i < row_count; i++) {
    matvec(col_count, red_count, &x[MINDEX2(row_count, col_count, i, 0)],
           &y[MINDEX2(row_count, red_count, i, 0)], w);
  }
}
void matmul(int row_count, int col_count, int red_count, float *const x,
            float *const y, float *const w) {
  for (int i = 0; i < row_count; i++) {
    for (int j = 0; j < col_count; j++) {
      y[MINDEX2(row_count, col_count, i, j)] = 0.f;
      for (int k = 0; k < red_count; k++) {
        y[MINDEX2(row_count, col_count, i, j)] +=
            x[MINDEX2(row_count, red_count, i, k)] *
            w[MINDEX2(col_count, red_count, j, k)];
      }
    }
  }
}
