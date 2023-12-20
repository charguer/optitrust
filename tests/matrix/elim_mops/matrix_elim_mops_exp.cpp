#include <optitrust.h>

void outer_alloc(int n, int m, int* p) {
  __modifies(
      "Group(range(0, n, 1), fun i1 -> Group(range(0, m, 1), fun i2 -> &p[i1 * "
      "m + i2] ~> Cell))");
  for (int i = 0; i < n; i++) {
    __modifies("Group(range(0, m, 1), fun j -> &p[i * m + j] ~> Cell)");
    for (int j = 0; j < m; j++) {
      __modifies("&p[i * m + j] ~> Cell");
      p[i * m + j] = p[i * m + j] + i + j;
    }
  }
}

void inner_alloc(int n, int m) {
  __pure();
  int* const p = (int* const)CALLOC2(n, m, sizeof(int));
  for (int i = 0; i < n; i++) {
    __modifies("Group(range(0, m, 1), fun j -> &p[i * m + j] ~> Cell)");
    for (int j = 0; j < m; j++) {
      __modifies("&p[i * m + j] ~> Cell");
      p[i * m + j] = p[i * m + j] + i + j;
    }
  }
  MFREE2(n, m, p);
}
