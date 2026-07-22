// Difficulty: Level 4
// Request: target the loop y whose body contains out[y]

void f(int n, float* out, float* tmp) {
  for (int y = 0; y < n; y++) {
    tmp[y] = y;
  }
  for (int y = 0; y < n; y++) {
    out[y] = tmp[y];
  }
}
