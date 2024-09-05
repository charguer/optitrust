#include <optitrust.h>
#include <stdio.h>

extern int N;

int main() {
  float* const a = (float*) MALLOC1(N, sizeof(float));
  float* const b = (float*) MALLOC1(N - 2, sizeof(float));
  for (int i = 0; i < N; i++) {
    a[MINDEX1(N, i)] = i;
    if (i >= 2) {
      b[MINDEX1(N-2, i-2)] = a[MINDEX1(N, i-2)] + a[MINDEX1(N, i-1)] + a[MINDEX1(N, i)];
    }
    if (i >= 4) {
      printf("%f\n", b[MINDEX1(N-2, i-4)] + b[MINDEX1(N-2, i-3)] + b[MINDEX1(N-2, i-2)]);
    }
  }

  MFREE1(N, a);
  MFREE1(N - 2, b);
  return 0;
}
