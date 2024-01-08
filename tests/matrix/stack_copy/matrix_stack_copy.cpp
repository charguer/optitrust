#include <optitrust.h>
// #include <stdio.h>

void f() {
  __pure();

  float* const s = (float* const) MALLOC2(32, 32, sizeof(float));

  for (int i = 0; i < 32; i++) {
    __writes("Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> Cell)");

    for (int j = 0; j < 32; j++) {
      __writes("&s[MINDEX2(32, 32, i, j)] ~> Cell");

      s[MINDEX2(32, 32, i, j)] = 0;
    }
  }

  for (int i = 0; i < 32; i++) {
    __modifies("Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> Cell)");

    // TODO? s[i][j] = x[j]
    for (int j = 0; j < 32; j++) {
      __modifies("&s[MINDEX2(32, 32, i, j)] ~> Cell");

      for (int k = 0; k < 4; k++) {
        __sequentially_modifies("&s[MINDEX2(32, 32, i, j)] ~> Cell");

        s[MINDEX2(32, 32, i, j)] += k;
      }
    }
  }

  for (int i = 0; i < 32; i++) {
    __reads("Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> Cell)");

    for (int j = 0; j < 32; j++) {
      __reads("&s[MINDEX2(32, 32, i, j)] ~> Cell");

      // TODO: printf("%f ", s[MINDEX2(32, 32, i, j)]);
    }
    // printf("\n");
  }

  MFREE2(32, 32, s);
}
