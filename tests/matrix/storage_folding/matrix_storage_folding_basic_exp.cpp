#include <optitrust.h>
#include <stdio.h>

int N;

typedef float T;

int main() {
  T* const a = (T*)MALLOC1(3, sizeof(T));
  T* const b = (T*)MALLOC1(3, sizeof(T));
  for (int i = 0; i < N; i++) {
    a[MINDEX1(3, i % 3)] = (float)i;
    if (i >= 2) {
      b[MINDEX1(3, (i - 2) % 3)] = a[MINDEX1(3, (i - 2) % 3)] +
                                   a[MINDEX1(3, (i - 1) % 3)] +
                                   a[MINDEX1(3, i % 3)];
    }
    if (i >= 4) {
      printf("%f\n",
             (double)(b[MINDEX1(3, (i - 4) % 3)] + b[MINDEX1(3, (i - 3) % 3)] +
                      b[MINDEX1(3, (i - 2) % 3)]));
    }
  }
  MFREE1(3, a);
  MFREE1(3, b);
  return 0;
}
