#include <math.h>

int const COLS = 100;

void gramSchmidt(float Q[][COLS], int const rows) {
  int cols = COLS;
#pragma omp target data[map(Q [0:rows] [0:cols])]
  for (int k = 0; (k < cols); k++) {
    double tmp = 0.;
#pragma omp target map(tofrom : tmp)
#pragma omp parallel for reduction(+ : tmp)
    for (int i = 0; (i < rows); i++)
      tmp += (Q[i][k] * Q[i][k]);
    tmp = (1 / sqrt(tmp));
#pragma omp target
#pragma omp parallel for
    for (int j = 0; (j < rows); j++)
      Q[j][k] *= tmp;
  }
}