void f(int n, float* out, float* tmp) {
  for (int y = 0; y < n; y++) {
    tmp[y] = y;
  }
  for (int y = 0; y < n; y++) {
    out[y] = tmp[y];
  }
}
