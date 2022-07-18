void init(float*, float*, int);

void output(float*, int);

void vec_mult(int N) {
  int i;
  float[N] p, v1, v2;
  init(v1, v2, N);
#pragma omp target
#pragma omp parallel for private(i)
  for (i = 0; i < N; i++) p[i] = v1[i] * v2[i];
  output(p, N);
}
