#include "optitrust_models.h"

void stuff(int *a, int N) {
  /*__writes("a ~~> 1");
  *a = 1;
  *a = 2;*/
  __requires("N: int, A: int -> int");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE1(N),0), MSIZE1(N)))");
  __consumes("DesyncGroup(range_plus(MINDEX1(MSIZE1(N),0), MSIZE1(N)), N, fun i -> &a[i] ~~> 0)");
  __produces("DesyncGroup(range_plus(MINDEX1(MSIZE1(N),0), MSIZE1(N)), N, fun i -> &a[i] ~~> 1)");
  for (int i = 0; i < N; i++) {
    __xconsumes("&a[i] ~~> 0");
    __xproduces("&a[i] ~~> 1");
    a[i] = 1;
  }
}
