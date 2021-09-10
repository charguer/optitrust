void work(int i) {}

void ordered_wrong(int n) {
#pragma omp ordered
  for (int i = 0; (i < n); i++) {
#pragma omp ordered
    work(i);
#pragma omp ordered
    work((i + 1));
  }
}