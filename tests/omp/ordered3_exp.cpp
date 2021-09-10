void work(int i) {}

void ordered_good(int n) {
#pragma omp for ordered
  for (int i = 0; (i < n); i++) {
    if ((i <= 10)) {
#pragma omp ordered
      work(i);
    }
    if ((i > 10)) {
#pragma omp ordered
      work((i + 1));
    }
  }
}