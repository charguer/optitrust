#include <optitrust.h>

typedef float T;

void simple(int N) {
  __pure();
  int x;
  for (int i = 0; i < N; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += i;
  }
  x++;
}

void ko(int n) {
  T* const c = (T*)MALLOC1(n, sizeof(T));
  for (int i = 0; i < n; i++) {
    c[MINDEX1(n, i)] = (float)i;
  }
  MFREE1(n, c);
}
