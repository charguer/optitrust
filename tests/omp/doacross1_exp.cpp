float foo(int i);

float bar(float a, float b);

float baz(float b);

void work(int N, float *A, float *B, float *C) {
#pragma omp for ordered(1)
  for (int i = 1; (i < N); i++) {
    A[i] = foo(i);
#pragma omp ordered depend(sink : i - 1)
    B[i] = bar(A[i], B[(i - 1)]);
#pragma omp ordered depend(source)
    C[i] = baz(B[i]);
  }
}