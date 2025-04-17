#include <optitrust.h>

void copy1from2(int* s) {
  __modifies("s ~> Matrix2(32, 32)");

  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");

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
}

void copy1from3(int* s) {
  __modifies("s ~> Matrix3(32, 32, 32)");

  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies("for j in 0..32 -> for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");

    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");

      for (int k = 0; k < 32; k++) {
        __strict();
        __xmodifies("&s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");

        s[MINDEX3(32, 32, 32, i, j, k)] += k;
      }
    }
  }
}

void copy2from3(int* s) {
  __modifies("s ~> Matrix3(32, 32, 32)");

  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies("for j in 0..32 -> for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");

    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("for k in 0..32 -> &s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");

      for (int k = 0; k < 32; k++) {
        __strict();
        __xmodifies("&s[MINDEX3(32, 32, 32, i, j, k)] ~> Cell");

        s[MINDEX3(32, 32, 32, i, j, k)] += k;
      }
    }
  }
}
