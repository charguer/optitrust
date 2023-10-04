#include "../../include/optitrust.h"

// NOTE: using pretty matrix notation

int* t;

int main() {
  for (int a = 0; a < 7; a++) {
    for (int b = 0; b < 10; b++) {
      for (int c = 0; c < 20; c++) {
        t[a] = b;
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    for (int j = i; j < i + 1; j++) {
    }
  }
}

void f(int* t, int* u, int* v, int n, int m) {
  __modifies("t ~> Matrix1(n)");
  __modifies("v ~> Matrix2(n, m)");
  __reads("u ~> Matrix1(n)");
  __ghost(
      rewrite,
      "H1 := Group(range(0, n, 1), fun x -> Group(range(0, m, 1), fun y -> "
      "&v[x][y] ~> Cell)), H2 := Group(range(0, m, 1), fun y -> Group(range(0, "
      "n, 1), fun x -> &v[x][y] ~> Cell)), by := swap_groups");
  for (int y = 0; y < m; y++) {
    __sequentially_modifies("t ~> Matrix1(n)");
    __sequentially_reads("u ~> Matrix1(n)");
    __modifies("Group(range(0, n, 1), fun x -> &v[x][y] ~> Cell)");
    for (int x = 0; x < n; x++) {
      __modifies("&t[x] ~> Cell");
      __modifies("&v[x][y] ~> Cell");
      __reads("&u[x] ~> Cell");
      t[x] = y * u[x];
      v[x][y] = t[x];
    }
  }
  __ghost(
      rewrite,
      "H1 := Group(range(0, m, 1), fun y -> Group(range(0, n, 1), fun x -> "
      "&v[x][y] ~> Cell)), H2 := Group(range(0, n, 1), fun x -> Group(range(0, "
      "m, 1), fun y -> &v[x][y] ~> Cell)), by := swap_groups");
}
