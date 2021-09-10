void init(float *, float *, int);

int maybe_init_again(float *, int);

void output(float *, int);

void vec_mult(float *p, float *v1, float *v2, int N) {
  init(v1, v2, N);
#pragma omp target data[map(to : v1[:N], v2[:N]); map(from : p0 [0:N])]
  {
    int changed;
    {
#pragma omp target
#pragma omp parallel for
      for (int i = 0; (i < N); i++)
        p[i] = (v1[i] * v2[i]);
      changed = maybe_init_again(v1, N);
#pragma omp target update if (changed), to(v1[:N])
    }
    changed = maybe_init_again(v2, N);
#pragma omp target update if (changed), to(v2[:N])
#pragma omp target
#pragma omp parallel for
    for (int j = 0; (j < N); j++)
      p[j] = (p[j] + (v1[j] * v2[j]));
  }
  output(p, N);
}