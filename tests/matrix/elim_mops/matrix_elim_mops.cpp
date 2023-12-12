#include <optitrust.h>

void outer_alloc(int n, int m, int* p) {
  __modifies("p ~> Matrix2(n, m)");

  for (int i = 0; i < n; i++){
    __modifies("Group(range(0, m, 1), fun j -> &p[MINDEX2(n, m, i, j)] ~> Cell)");

    for (int j = 0; j < m; j++){
      __modifies("&p[MINDEX2(n, m, i, j)] ~> Cell");

      p[MINDEX2(n, m, i, j)] = p[MINDEX2(n, m, i, j)] + i + j;
    }
  }
}

void inner_alloc(int n, int m) {
  __pure();

  int* const p = (int* const) CALLOC2(n, m, sizeof(int));
  for (int i = 0; i < n; i++){
    __modifies("Group(range(0, m, 1), fun j -> &p[MINDEX2(n, m, i, j)] ~> Cell)");

    for (int j = 0; j < m; j++){
      __modifies("&p[MINDEX2(n, m, i, j)] ~> Cell");

      p[MINDEX2(n, m, i, j)] = p[MINDEX2(n, m, i, j)] + i + j;
    }
  }
  MFREE2(n, m, p);
}
