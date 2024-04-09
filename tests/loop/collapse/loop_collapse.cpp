#include <optitrust.h>

void consts() {
  __pure();

  int x;
  for (int i = 0; i < 5; i++) {
    for (int j = 0; j < 5; j++) {
      x += i + j;
    }
  }
}

void from_zero(int n, int m) {
  // __pure();
  __requires("n >= 0, m >= 0");

  int x;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      x += i + j;
    }
  }
}

void from_zero_contract(int* t, int* u, int n, int m) {
  __requires("n >= 0, m >= 0");
  __reads("u ~> Matrix2(n, m)");
  __writes("t ~> Matrix2(n, m)");

  for (int i = 0; i < n; i++) {
    __xwrites("for j in 0..m -> &t[MINDEX2(n, m, i, j)] ~> Cell");
    for (int j = 0; j < m; j++) {
      __xwrites("&t[MINDEX2(n, m, i, j)] ~> Cell");
      __GHOST_BEGIN(f, matrix2_ro_focus, "u, i, j");
      t[MINDEX2(n, m, i, j)] = u[MINDEX2(n, m, i, j)];
      __GHOST_END(f);
    }
  }
}

void from_zero_wrong(int n, int m) {
  // __pure();
  __requires("n >= 0, m >= 0");

  int x;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < m; j++) {
      x += i + j;
    }
  }
}
