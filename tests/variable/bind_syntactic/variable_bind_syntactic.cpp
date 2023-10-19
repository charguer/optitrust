#include "../../include/optitrust.h"

int main() {

  int s = 0;
  for (int i = 0; i < 3; i++) {
    s = 2 + 3;
  }

  int t = 0;
  for (int j = 0; j < 3; j++) {
    t += 2 + s;
    t += 2 + s;
  }

  int r = (4 + 3) * (4 + 3) + (4 + 4) * (4 + 4);

  float* const m = (float* const) MALLOC2(5, 6, sizeof(float));
  float x = m[MINDEX2(5, 6, 0, 1)] * m[MINDEX2(5, 6, 1, 0)];
  MFREE2(5, 6, m);

  return 0;
}

