void init(float *, int);

void output(float *, int);

void vec_mult(int N) {
  int i;
#pragma omp parallel num_threads(2)
  float[N] p, v1, v2;
  {
#pragma omp single
    {
#pragma omp task depend(out : v1)
      init(v1, N);
#pragma omp task depend(out : v2)
      init(v2, N);
#pragma omp target nowait,                                                     \
    depend(in                                                                  \
           : v1, v2),                                                          \
    depend(out                                                                 \
           : p),                                                               \
    map(to                                                                     \
        : v1, v2),                                                             \
    map(from                                                                   \
        : p)
#pragma omp parallel for private(i)
      for (int i = 0; (i < N); i++)
        p[i] = (v1[i] * v2[i]);
#pragma omp task depend(in : p)
      output(p, N);
    }
  }
}