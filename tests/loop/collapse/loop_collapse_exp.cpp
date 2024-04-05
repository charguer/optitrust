#include <optitrust.h>

void consts() {
  __pure();
  int x;
  for (int ij = 0; ij < 5 * 5; ij++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += ij / 5 + ij % 5;
  }
}

void from_zero(int n, int m) {
  __requires("__is_geq(n, 0)");
  __requires("__is_geq(m, 0)");
  int x;
  for (int ij = 0; ij < n * m; ij++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += ij / m + ij % m;
  }
}

void from_zero_wrong(int n, int m) {
  __requires("__is_geq(n, 0)");
  __requires("__is_geq(m, 0)");
  int x;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __smodifies("&x ~> Cell");
      x += i + j;
    }
  }
}
