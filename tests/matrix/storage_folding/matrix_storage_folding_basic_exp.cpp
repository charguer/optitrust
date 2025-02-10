#include <optitrust.h>
#include <stdio.h>

extern int N;

int main() {
  float* const a = (float*)malloc(MSIZE1(3) * sizeof(float));
  float* const b = (float*)malloc(MSIZE1(3) * sizeof(float));
  for (int i = 0; i < N; i++) {
    a[MINDEX1(3, i % 3)] = i;
    if (i >= 2) {
      b[MINDEX1(3, (i - 2) % 3)] = a[MINDEX1(3, (i - 2) % 3)] +
                                   a[MINDEX1(3, (i - 1) % 3)] +
                                   a[MINDEX1(3, i % 3)];
    }
    if (i >= 4) {
      printf("%f\n", b[MINDEX1(3, (i - 4) % 3)] + b[MINDEX1(3, (i - 3) % 3)] +
                         b[MINDEX1(3, (i - 2) % 3)]);
    }
  }
  free(a);
  free(b);
  return 0;
}
