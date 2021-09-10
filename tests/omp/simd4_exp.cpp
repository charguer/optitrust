void work(float *b, int n, int m) {
#pragma omp simd safelen(16)
  for (int i = m; (i < n); i++)
    b[i] = (b[(i - m)] - 1.);
}