void work(int i, int j) {}

void work1(int i, int n) {
#pragma omp for
  for (int j = 0; (j < n); j++)
    work(i, j);
}

void wrong2(int n) {
#pragma omp parallel default(shared)
  {
#pragma omp for
    for (int i = 0; (i < n); i++)
      work1(i, n);
  }
}