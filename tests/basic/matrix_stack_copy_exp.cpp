#include <stdio.h>

#include "../../include/optitrust.h"

int main() {
  float* const s = (float* const)MALLOC2(32, 32, sizeof(float));
  for (int i = 0; i < 32; i++) {
    float x[32];
    memcpy(x, &s[MINDEX2(32, 32, i, 0)], sizeof(float[32]));
    for (int j = 0; j < 32; j++) {
      for (int k = 0; k < 4; k++) {
        x[j] += k;
      }
    }
    memcpy(&s[MINDEX2(32, 32, i, 0)], x, sizeof(float[32]));
  }
  for (int i = 0; i < 32; i++) {
    for (int j = 0; j < 32; j++) {
      printf("%f ", s[MINDEX2(32, 32, i, j)]);
    }
    printf("\n");
  }
  MFREE2(32, 32, s);
  return 0;
}
