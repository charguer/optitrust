#include <optitrust.h>

typedef float T;

void simple(int N) {
  __pure();
  int x = 0;
  for (int i = 0; i < N; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += i;
  }
  x++;
}

void ko(int n) {
  T* const c = (T*)malloc(MSIZE1(n) * sizeof(T));
  for (int i = 0; i < n; i++) {
    c[MINDEX1(n, i)] = i;
  }
  free(c);
}
