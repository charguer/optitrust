#include <limits.h>

#include <math.h>

void reduction2(float *x, int *y, int n) {
  int b, b_p, c, c_p;
  float a, a_p, d, d_p;
  a = 0.;
  b = 0;
  c = y[0];
  d = x[0];
#pragma omp parallel shared(a, b, c, d, x, y, n) private(a_p, b_p, c_p, d_p)
  {
    a_p = 0.;
    b_p = 0;
    c_p = 2147483647;
    d_p = (-__builtin_huge_valf());
#pragma omp for private(i)
    for (int i = 0; (i < n); i++) {
      a_p += x[i];
      b_p ^= y[i];
      if ((c_p > y[i]))
        c_p = y[i];
      d_p = fmaxf(d_p, x[i]);
    }
#pragma omp critical
    {
      a += a_p;
      b ^= b_p;
      if ((c > c_p))
        c = c_p;
      d = fmaxf(d, d_p);
    }
  }
}