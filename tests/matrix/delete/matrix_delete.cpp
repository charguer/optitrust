#include <stdio.h>
#include "../../include/optitrust.h"

extern int N;
typedef float T;

int main() {
  T* const a = (T* const) MALLOC1(N, sizeof(T));
  T* const b = (T* const) MALLOC1(N - 2, sizeof(T));
  for (int i = 0; i < N; i++) {
    printf("%i\n", i);
  }

  MFREE1(N, a);
  MFREE1(N - 2, b);
  return 0;
}