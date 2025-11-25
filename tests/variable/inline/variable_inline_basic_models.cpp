#include <optitrust_models.h>

void into_loop(int* a, int* cst, int n) {
  __requires("A: int -> int");
  __requires("C: int");
  __consumes("for i in 0..n -> &a[i] ~~> A(i)");
  __produces("for i in 0..n -> &a[i] ~~> A(i) + C");
  __reads("cst ~~> C");

  const int x = *cst;
  for (int i = 0; i < n; i++) {
    __xconsumes("&a[i] ~~> A(i)");
    __xproduces("&a[i] ~~> A(i) + C");
    __requires("xC: x = C");

    a[i] += x;
  }
}
