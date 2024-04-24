#include <optitrust.h>

void consts() {
  __pure();
  int x;
  __ghost(assume, "F := __is_geq(5, 0)");
  __ghost(assume, "F := __is_geq(5, 0)");
  for (int ij = 0; ij < 5 * 5; ij++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "F := in_range(ij / 5, 0..5)");
    __ghost(assume, "F := in_range(ij % 5, 0..5)");
    x += ij / 5 + ij % 5;
  }
}

void from_zero(int n, int m) {
  __requires("__is_geq(n, 0)");
  __requires("__is_geq(m, 0)");
  int x;
  __ghost(assume, "F := __is_geq(n, 0)");
  __ghost(assume, "F := __is_geq(m, 0)");
  for (int ij = 0; ij < n * m; ij++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "F := in_range(ij / m, 0..n)");
    __ghost(assume, "F := in_range(ij % m, 0..m)");
    x += ij / m + ij % m;
  }
}

void from_zero_contract(int* t, int* u, int n, int m) {
  __requires("__is_geq(n, 0)");
  __requires("__is_geq(m, 0)");
  __writes("t ~> Matrix2(n, m)");
  __reads("u ~> Matrix2(n, m)");
  __ghost(assume, "F := __is_geq(n, 0)");
  __ghost(assume, "F := __is_geq(m, 0)");
  __ghost(
      group_collapse_uninit,
      "n := n, m := m, items := fun i, j -> &t[MINDEX2(n, m, i, j)] ~> Cell");
  for (int ij = 0; ij < n * m; ij++) {
    __strict();
    __sreads("u ~> Matrix2(n, m)");
    __xwrites("&t[MINDEX2(n, m, ij / m, ij % m)] ~> Cell");
    __ghost(assume, "F := in_range(ij / m, 0..n)");
    __ghost(assume, "F := in_range(ij % m, 0..m)");
    const __ghost_fn f =
        __ghost_begin(matrix2_ro_focus, "M := u, i := ij / m, j := ij % m");
    t[MINDEX2(n, m, ij / m, ij % m)] = u[MINDEX2(n, m, ij / m, ij % m)];
    __ghost_end(f);
  }
  __ghost(
      group_uncollapse,
      "n := n, m := m, items := fun i, j -> &t[MINDEX2(n, m, i, j)] ~> Cell");
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
