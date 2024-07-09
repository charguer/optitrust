#include <optitrust.h>
#include <stdio.h>

int main() {
  float* s = (float*)MALLOC1(32, sizeof(float));

  for (int i = 0; i < 32; i++) {
    s[MINDEX1(32, i)] = 0.0f;
  }

  for (int j = 0; j < 30; j++) {
    printf("%f\n", s[MINDEX1(32, j - 2)]);
  }

  for (int k = 0; k < 15; k += 3) {
    s[MINDEX1(32, k)] = 1.0;
  }

  free(s);
  return 0;
}
