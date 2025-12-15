#include "optitrust_models.h"

void stuff(int *a, int *s, int N) {
  /*__writes("a ~~> 1");
  *a = 1;
  *a = 2;*/
  __requires("N: int, A: int -> int");
  __preserves("ThreadsCtx(range_plus(0, N))");
  __preserves("DesyncGroup(range_plus(0,N), N, fun i -> &a[i] ~~> A(i))");
  __writes("s ~~> 0");
  *s = 0;
  for (int i = 0; i < N; i++) {
    __spreserves("s ~~> 0");
    *s = 1;
    *s = 0;
  }
}
