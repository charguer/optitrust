// Difficulty: Level 4
// Request: target every y loop that writes to ix or iy

void smooth(int n, float* ix, float* iy, float* out) {
  for (int y = 0; y < n; y++) {
    ix[y] = out[y] + 1.0f;
  }
  for (int y = 0; y < n; y++) {
    iy[y] = out[y] + 2.0f;
  }
  for (int y = 0; y < n; y++) {
    out[y] = ix[y] + iy[y];
  }
}
