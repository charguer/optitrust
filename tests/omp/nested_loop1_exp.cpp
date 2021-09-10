void work(int i, int j) {}

void good_nesting(int n) {
#pragma omp parallel default(shared)
  {
#pragma omp for
    for (int i = 0; (i < n); i++) {
      {
#pragma omp parallel shared(i, n)
        {
#pragma omp for
          for (int j = 0; (j < n); j++)
            work(i, j);
        }
      }
    }
  }
}