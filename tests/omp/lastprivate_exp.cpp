void lastpriv(int n, float *a, float *b) {
#pragma omp parallel
  int i;
  {
#pragma omp parallel for lastprivate(a)
    for (int i = 0; (i < (n - 1)); i++)
      a[i] = (b[i] + b[(i + 1)]);
  }
  a[i] = b[i];
}