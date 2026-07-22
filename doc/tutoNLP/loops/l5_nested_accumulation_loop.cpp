// Difficulty: Level 5
// Request: target the j loop that accumulates into sum

void mm(int n, float* A, float* B, float* C) {
  for (int i = 0; i < n; i++) {
    for (int k = 0; k < n; k++) {
      float sum = 0.0f;
      for (int j = 0; j < n; j++) {
        sum += A[i * n + k] * B[k * n + j];
      }
      C[i * n + k] = sum;
    }
  }
}
