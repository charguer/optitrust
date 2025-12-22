#include <optitrust_models.h>

void f(int *a, int *b, int N) {
  __requires("N: int");
  __consumes("for i in 0..N -> &a[MINDEX1(N,i)] ~~> 0");
  __produces("for i in 0..N -> &a[MINDEX1(N,i)] ~~> 2");
  __preserves("b ~~> 0");
  for (int i = 0; i < N; i++) {
    __strict();
    __spreserves("b ~~> 0");
    __xconsumes("&a[MINDEX1(N,i)] ~~> 0");
    __xproduces("&a[MINDEX1(N,i)] ~~> 2");
    a[MINDEX1(N,i)] = 1;
    a[MINDEX1(N,i)] = 2;
  }
}
