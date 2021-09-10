#include <stdio.h>

float average(float, float, float);

void master_example(float *x, float *xold, int n, float tol) {
  int c, toobig;
  float error, y;
  c = 0;
  {
    do {
#pragma omp for private(i)
      for (int i = 1; (i < (n - 1)); ++i) {
        xold[i] = x[i];
      }
#pragma omp single
      { toobig = 0; }
#pragma omp for private(i, y, error) reduction(+ : toobig)
      for (int i = 1; (i < (n - 1)); ++i) {
        y = x[i];
        x[i] = average(xold[(i - 1)], x[i], xold[(i + 1)]);
        error = (y - x[i]);
        if (((error > tol) || (error < (-tol))))
          ++toobig;
      }
#pragma omp master
      {
        ++c;
        printf("iteration %d, toobig=%d\n", c, toobig);
      }
    } while ((toobig > 0));
  }
}