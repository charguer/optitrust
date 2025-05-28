#include <optitrust.h>

int main() {
  const int N = 1;
  const int M = 3;
  double x[N * M];
  MATRIX2_MEMSET_double(x, 25, N, M);
  MATRIX1_MEMSET_double(x, 25, N * M);
  for (int k = 0; k < N * M; k++) {
    x[MINDEX1(M * N, k)] = 25;
  }
  for (int k = 1; k < N * M; k++) {
    x[MINDEX1(N * M, k)] = 25;
  }
  for (int k = 0; k < N * M - 1; k += 2) {
    x[MINDEX1(N * M, k)] = 25;
  }
  for (int k = 0; k < N * M; k += 2) {
    int j = 12 + k;
  }
}
