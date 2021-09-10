void work(int i, int j) {}

void wrong1(int n) {
#pragma omp parallel default(shared)
  {
#pragma omp for
    for (int i = 0; (i < n); i++) {
#pragma omp for
      for (int j = 0; (j < n); j++)
        work(i, j);
    }
  }
}