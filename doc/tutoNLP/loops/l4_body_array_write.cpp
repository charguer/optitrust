// Difficulty: Level 4
// Request: target the y loop that writes to out

void harris_like(int n, float* tmp, float* out) {
  for (int y = 0; y < n; y++) {
    tmp[y] = y;
  }
  for (int y = 0; y < n; y++) {
    out[y] = tmp[y];
  }
}
