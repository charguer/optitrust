int atomic_read(int const *p) {
  int value;
#pragma omp atomic read
  value = (*p);
  return value;
}

void atomic_write(int *p, int value) {
#pragma omp atomic write
  (*p) = value;
}