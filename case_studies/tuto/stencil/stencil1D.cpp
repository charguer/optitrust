const int N = 51200000;

// void stencil1D(double* a, double* b, int N) {
void stencil1D(double a[N], double b[N]) {
  for (int i = 1; i < N; i++) {
    // a[MINDEX1(N, i)]
    a[i] = (a[i] + a[i - 1]) / 2;
    b[i] = (b[i] + b[i - 1]) / 2;
  }
}

