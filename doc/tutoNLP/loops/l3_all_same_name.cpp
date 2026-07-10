void blur(float* out) {
  for (int x = 0; x < 4; x++) {
    out[x] = 0.0f;
  }
  for (int y = 0; y < 4; y++) {
    for (int x = 0; x < 4; x++) {
      out[y * 4 + x] += 1.0f;
    }
  }
}
