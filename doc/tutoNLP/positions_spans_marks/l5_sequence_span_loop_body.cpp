// Difficulty: Level 5
// Request: target the whole sequence inside the bi loop

void dot(float* a, float* b, float* out) {
  for (int bi = 0; bi < 128; bi++) {
    float t = 0.0f;
    t += a[bi] * b[bi];
    out[bi] = t;
  }
}
