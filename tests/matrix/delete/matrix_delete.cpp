#include <optitrust.h>
// #include <stdio.h>

typedef float T;

void simple(int N) {
  __pure();

  int x = 0;
  T* const a = MALLOC1(T, N);
  T* const b = MALLOC1(T, N - 2);
  for (int i = 0; i < N; i++) {
    // printf("%i\n", i);
    x += i;
  }

  free(a);
  free(b);
  x++;
}

void ko(int n) {
  T* const c = MALLOC1(T, n);
  for (int i = 0; i < n; i++) {
    c[MINDEX1(n, i)] = i;
  }
  free(c);
}
