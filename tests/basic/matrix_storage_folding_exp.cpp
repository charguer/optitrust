#include <stdio.h>

#include "../../include/optitrust.h"

int N;

typedef float T;

int main() {
  T* a = (T*)MALLOC1(3, sizeof(T));
  T* b = (T*)MALLOC1(3, sizeof(T));
  for (int i = 0; i < N; i++) {
    a[MINDEX1(3, i % 3)] = i;
    if (i >= 2) {
      b[MINDEX1(3, (i - 2) % 3)] = a[MINDEX1(3, (i - 2) % 3)] +
                                   a[MINDEX1(3, (i - 1) % 3)] +
                                   a[MINDEX1(3, i % 3)];
    }
    if (i >= 4) {
      printf("%i\n", b[MINDEX1(3, (i - 4) % 3)] + b[MINDEX1(3, (i - 3) % 3)] +
                         b[MINDEX1(3, (i - 2) % 3)]);
    }
  }
  free(a);
  free(b);
  return 0;
}
