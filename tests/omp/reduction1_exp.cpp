#include <math.h>

void reduction1(float *x, int *y, int n) {
  int b, c;
  float a, d;
  a = 0.;
  b = 0;
  c = y[0];
  d = x[0];
#pragma omp parallel for private(i)  shared(x, y, n)  reduction(+:a)  reduction(^:b)  reduction(min:c)  reduction(max:d)  lastprivate(a)
  for (int i = 0; (i < n); i++) {
    a += x[i];
    b ^= y[i];
    if ((c > y[i]))
      c = y[i];
    d = fmaxf(d, x[i]);
  }
}