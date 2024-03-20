#include <optitrust.h>

void outer_alloc(int n, int m, int* p) {
  __modifies("for i1 in 0..n -> for i2 in 0..m -> &p[i1 * m + i2] ~> Cell");
  for (int i = 0; i < n; i++) {
    __strict();
    __modifies("for j in 0..m -> &p[i * m + j] ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __modifies("&p[i * m + j] ~> Cell");
      p[i * m + j] = p[i * m + j] + i + j;
    }
  }
}

void inner_alloc(int n, int m) {
  __pure();
  int* const p = (int* const)CALLOC2(n, m, sizeof(int));
  for (int i = 0; i < n; i++) {
    __strict();
    __modifies("for j in 0..m -> &p[i * m + j] ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __modifies("&p[i * m + j] ~> Cell");
      p[i * m + j] = p[i * m + j] + i + j;
    }
  }
  MFREE2(n, m, p);
}
