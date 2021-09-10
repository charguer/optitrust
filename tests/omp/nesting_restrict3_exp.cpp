void work(int i, int j) {}

void wrong3(int n) {
#pragma omp parallel default(shared)
  {
#pragma omp for
    for (int i = 0; (i < n); i++) {
#pragma omp single
      work(i, 0);
    }
  }
}