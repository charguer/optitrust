// Difficulty: Level 5
// Request: target the instruction span around the first write to d_partial_sums

void f(float* d_partial_sums) {
  d_partial_sums[0] = 0.0f;
  d_partial_sums[1] = 1.0f;
}
