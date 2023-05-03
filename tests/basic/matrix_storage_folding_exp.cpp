#include <stdio.h>

#include "../../include/optitrust.h"

int N;

typedef float T;

void main() {
  T a[N];
  for (int i = 0; i < N; i++) {
    a[i] = i;
  }
  T b[N - 2];
  for (int i = 0; i < N - 2; i++) {
    b[i] = a[i] + a[i + 1] + a[i + 1];
  }
  for (int i = 0; i < N - 4; i++) {
    printf("%i\n", b[i] + b[i + 1] + b[i + 2]);
  }
}
