#include "../../include/optitrust.h"
#include <stdio.h>

int main() {
  float* s = (float*)MALLOC2(32, 32, sizeof(float));
  // s[32][32] = { 0 };

  for (int i = 0; i < 32; i++) {
    // TODO: s[i][j] = x[j]
    for (int j = 0; j < 32; j++) {
      for (int k = 0; k < 4; k++) {
        s[MINDEX2(32, 32, i, j)] += k;
      }
    }
  }

  for (int i = 0; i < 32; i++) {
    for (int j = 0; j < 32; j++) {
      printf("%f ", s[MINDEX2(32, 32, i, j)]);
    }
    printf("\n");
  }

  free(s);
  return 0;
}