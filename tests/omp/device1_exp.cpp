#include <stdio.h>

#include <omp.h>

void vec_mult(float *p, float *v1, float *v2, int N);

float *p;

float *v1;

float *v2;

int N;

#pragma omp end declare target

void init_vars(float *, float *, int);

void output(float *, int);

void foo() {
  init_vars(v1, v2, N);
#pragma omp target device(42), map(p[:N], v1[:N], v2[:N]])
  {
#pragma omp declare target
    vec_mult(p, v1, v2, N);
  }
  output(p, N);
}

void vec_mult(float *p, float *v1, float *v2, int N) {
  int nthreads;
  if ((!omp_is_initial_device())) {
    printf("1024 threads on target device\n");
    nthreads = 1024;
  } else {
    printf("8 threads on initial device\n");
    nthreads = 8;
  }
#pragma omp parallel for private(i) num_threads(nthreads)
  for (int i = 0; (i < N); i++)
    p[i] = (v1[i] * v2[i]);
}