#include <optitrust.h>
// #include <stdio.h>

typedef float T;

void simple(int N) {
  __pure();

  int x;
  T* const a = (T* const) MALLOC1(N, sizeof(T));
  T* const b = (T* const) MALLOC1(N - 2, sizeof(T));
  for (int i = 0; i < N; i++) {
    // printf("%i\n", i);
    x += i;
  }

  MFREE1(N, a);
  MFREE1(N - 2, b);
  x++;
}

void ko(int n) {
  T* const c = (T* const) MALLOC1(n, sizeof(T));
  for (int i = 0; i < n; i++) {
    c[MINDEX1(n, i)] = i;
  }
  MFREE1(n, c);
}
