union {
  int n;
  float x;
} u;

void atomic_wrong() {
#pragma omp parallel
  {
#pragma omp atomic update
    u.n++;
#pragma omp atomic update
    u.x += 1.;
  }
}