void init(float *, float *, int);

void init_again(float *, float *, int);

void output(float *, int);

void vec_mult(float *p, float *v1, float *v2, int N) {
  init(v1, v2, N);
#pragma omp target data[map(from : p [0:N])]
  {
#pragma omp target map(to : v1[:N], v2[N])
#pragma omp parallel for
    for (int i = 0; (i < N); i++)
      p[i] = (v1[i] * v2[i]);
    init_again(v1, v2, N);
#pragma omp target map(to : v1[:N], v2[N])
#pragma omp parallel for
    for (int j = 0; (j < N); j++)
      p[j] = (p[j] + (v1[j] * v2[j]));
  }
  output(p, N);
}