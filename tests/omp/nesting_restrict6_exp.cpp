void work(int i, int j) {}

void wrong6(int n) {
#pragma omp parallel
  {
#pragma omp single
    {
      work(n, 0);
#pragma omp barrier
      work(n, 1);
    }
  }
}