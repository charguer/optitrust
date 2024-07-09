#include <optitrust.h>
// #include <stdio.h>

void f() {
  __pure();

  float* const s = (float*) MALLOC2(32, 32, sizeof(float));

  for (int i = 0; i < 32; i++) {
    __strict();
    __xwrites("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");

    for (int j = 0; j < 32; j++) {
      __strict();
      __xwrites("&s[MINDEX2(32, 32, i, j)] ~> Cell");

      s[MINDEX2(32, 32, i, j)] = 0;
    }
  }

  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");

    // TODO? s[i][j] = x[j]
    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("&s[MINDEX2(32, 32, i, j)] ~> Cell");

      for (int k = 0; k < 4; k++) {
        __strict();
        __smodifies("&s[MINDEX2(32, 32, i, j)] ~> Cell");

        s[MINDEX2(32, 32, i, j)] += k;
      }
    }
  }

  for (int i = 0; i < 32; i++) {
    __strict();
    __xreads("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");

    for (int j = 0; j < 32; j++) {
      __strict();
      __xreads("&s[MINDEX2(32, 32, i, j)] ~> Cell");

      // TODO: printf("%f ", s[MINDEX2(32, 32, i, j)]);
    }
    // printf("\n");
  }

  MFREE2(32, 32, s);
}
