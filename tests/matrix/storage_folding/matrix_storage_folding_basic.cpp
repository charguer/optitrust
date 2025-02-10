#include <optitrust.h>
#include <stdio.h>

extern int N;

int main() {
  float* const a = MALLOC1(float, N);
  float* const b = MALLOC1(float, N - 2);
  for (int i = 0; i < N; i++) {
    a[MINDEX1(N, i)] = i;
    if (i >= 2) {
      b[MINDEX1(N-2, i-2)] = a[MINDEX1(N, i-2)] + a[MINDEX1(N, i-1)] + a[MINDEX1(N, i)];
    }
    if (i >= 4) {
      printf("%f\n", b[MINDEX1(N-2, i-4)] + b[MINDEX1(N-2, i-3)] + b[MINDEX1(N-2, i-2)]);
    }
  }

  free(a);
  free(b);
  return 0;
}
