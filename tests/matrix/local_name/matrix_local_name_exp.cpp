#include "../../include/optitrust.h"

typedef int T;

T* b;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* a = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  T* x = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        x[MINDEX3(N1, N2, N3, i1, i2, i3)] = a[MINDEX3(N1, N2, N3, i1, i2, i3)];
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    x[MINDEX3(N1, N2, N3, i, i + 1, i + 2)];
  }
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        a[MINDEX3(N1, N2, N3, i1, i2, i3)] = x[MINDEX3(N1, N2, N3, i1, i2, i3)];
      }
    }
  }
  MFREE3(N1, N2, N3, x);
  b = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  T* y = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        y[MINDEX3(N1, N2, N3, i1, i2, i3)] = b[MINDEX3(N1, N2, N3, i1, i2, i3)];
      }
    }
  }
  for (int j = 0; j < 10; j++) {
    y[MINDEX3(N1, N2, N3, j, j + 1, j + 2)];
  }
  for (int i1 = 0; i1 < N1; i1++) {
    for (int i2 = 0; i2 < N2; i2++) {
      for (int i3 = 0; i3 < N3; i3++) {
        b[MINDEX3(N1, N2, N3, i1, i2, i3)] = y[MINDEX3(N1, N2, N3, i1, i2, i3)];
      }
    }
  }
  MFREE3(N1, N2, N3, y);
  int z = 0;
  return 0;
}
