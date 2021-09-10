#include <stdlib.h>

#include <omp.h>

#pragma omp declare target

void init(float *, float *, int);

#pragma omp end declare target

void foo();

void output(float *, int);

void vec_mult(float *p, int N, int dev) {
  float *v1, v2;
#pragma omp task shared(v1, v2) depend(out : v1, v2)
#pragma omp target device(dev), map(v1, v2)
  int i;
  {
    if (omp_is_initial_device())
      abort();
    v1 = (float *)malloc((N * sizeof(float)));
    v2 = (float *)malloc((N * sizeof(float)));
    init(v1, v2, N);
  }
  foo();
#pragma omp task shared(v1, v2, p) depend(in : v1, v2)
#pragma omp target device(dev), map(to : v1, v2), map(from : p [0:N])
  {
    if (omp_is_initial_device())
      abort();
#pragma omp parallel for
    for (int i = 0; (i < N); i++)
      p[i] = (v1[i] * v2[i]);
    free(v1);
    free(v2);
  }
#pragma omp taskwait
  output(p, N);
}