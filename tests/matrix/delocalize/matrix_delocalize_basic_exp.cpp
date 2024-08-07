#include <optitrust.h>

typedef int T;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 11;
  const int N3 = 12;
  T *a = (T *)CALLOC3(N1, N2, N3, sizeof(T));
mark : {
  T *x = (T *)CALLOC4(N0, N1, N2, N3, sizeof(T));
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        for (int i0 = 0; i0 < N0; i0++) {
          x[MINDEX4(N0, N1, N2, N3, i0, i1, i2, i3)] = 0;
        }
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    int t = x[MINDEX4(N0, N1, N2, N3, ANY(N0), i, i + 1, i + 2)];
    x[MINDEX4(N0, N1, N2, N3, ANY(N0), i, i + 1, i + 2)] = t + 2;
  }
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        double sum = 0;
        for (int i0 = 0; i0 < N0; i0++) {
          sum += x[MINDEX4(N0, N1, N2, N3, i0, i1, i2, i3)];
        }
        a[MINDEX3(N1, N2, N3, i1, i2, i3)] = sum;
      }
    }
  }
  MFREE(x);
}
  int y = 0;
  return 0;
}
