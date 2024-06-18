#include <optitrust.h>
#include <stdio.h>

int main() {
  float* s = (float*)MALLOC1(32, sizeof(float));
  for (int bi = 0; bi < 32 + (1 - 2); bi += 1) {
    for (int i = bi; i < bi + 2; i++) {
      s[MINDEX1(32, i)] = 0.f;
    }
  }
  for (int bj = 0; bj < 30 + (2 - 2); bj += 2) {
    for (int j = bj; j < min(30, bj + 2); j++) {
      printf("%f\n", (double)s[MINDEX1(32, j - 2)]);
    }
  }
  for (int bk = 0; bk < 15 + (1 - 2) * 3; bk += 1 * 3) {
    for (int k = bk; k < min(15, bk + 2 * 3); k += 3) {
      s[MINDEX1(32, k)] = 1.f;
    }
  }
  free(s);
  return 0;
}
