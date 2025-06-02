const int N = 5000;

void polynomial_multiply(double c[2 * N - 1], double a[N], double b[N]) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      for (int k = 0; k < 2 * N + 1; k++) {
        if (k == i + j) {
          c[k] += a[i] * b[j];
        }
      }
    }
  }
}
