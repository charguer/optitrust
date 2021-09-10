void work(int i, int j) {}

void wrong4(int n) {
#pragma omp parallel default(shared)
  {
#pragma omp for
    for (int i = 0; (i < n); i++) {
      work(i, 0);
#pragma omp barrier
      work(i, 1);
    }
  }
}