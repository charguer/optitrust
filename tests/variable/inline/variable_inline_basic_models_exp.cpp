#include <optitrust_models.h>

void into_loop(int* a, int* cst, int n) {
  __requires("A: int -> int");
  __requires("C: int");
  __consumes("for i in 0..n -> &a[i] ~~> A(i)");
  __produces("for i in 0..n -> &a[i] ~~> A(i) + C");
  __reads("cst ~~> C");
  for (int i = 0; i < n; i++) {
    __strict();
    __requires("xC: __is_true(C == C)");
    __sreads("cst ~~> C");
    __xconsumes("&a[i] ~~> A(i)");
    __xproduces("&a[i] ~~> A(i) + C");
    a[i] += *cst;
  }
}
