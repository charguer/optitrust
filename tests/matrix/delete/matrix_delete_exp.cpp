#include <optitrust.h>

typedef float T;

void simple(int N) {
__pure();
  int x;
  for (int i = 0; i < N; i++) {
    __sequentially_modifies("&x ~> Cell");
    x += i;
  }
  x++;
}

void ko(int n) {
  T* const c = (T* const)MALLOC1(n, sizeof(T));
  for (int i = 0; i < n; i++) {
    c[MINDEX1(n, i)] = i;
  }
  MFREE1(n, c);
}
