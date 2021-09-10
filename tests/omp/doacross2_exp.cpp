float foo(int i, int j);

float bar(float a, float b, float c);

float baz(float b);

void work(int N, int M, float **A, float **B, float **C) {
#pragma omp for ordered(2)
  for (int i = 1; (i < N); i++) {
    for (int j = 1; (j < M); j++) {
      A[i][j] = foo(i, j);
#pragma omp ordered depend(sink : i - 1, j) depend(sink : i, j - 1)
      B[i][j] = bar(A[i][j], B[(i - 1)][j], B[i][(j - 1)]);
#pragma omp ordered depend(source)
      C[i][j] = baz(B[i][j]);
    }
  }
}