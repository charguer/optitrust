#include "../../../include/optitrust.h"

typedef int T;

T* b;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* const a = (T* const)MALLOC3(N1, N2, N3, sizeof(T));
  T* const x = (T* const)MALLOC3(10, 8, 4, sizeof(T));
  for (int i1 = 0; i1 < 0 + 10; i1++) {
    for (int i2 = 2; i2 < 2 + 8; i2++) {
      for (int i3 = 0; i3 < 0 + 4; i3++) {
        x[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)] =
            a[MINDEX3(N1, N2, N3, i1, i2, i3)];
      }
    }
  }
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        x[MINDEX3(10, 8, 4, i - 0, j - 2, k - 0)] = 1;
      }
    }
  }
  for (int i1 = 0; i1 < 0 + 10; i1++) {
    for (int i2 = 2; i2 < 2 + 8; i2++) {
      for (int i3 = 0; i3 < 0 + 4; i3++) {
        a[MINDEX3(N1, N2, N3, i1, i2, i3)] =
            x[MINDEX3(10, 8, 4, i1 - 0, i2 - 2, i3 - 0)];
      }
    }
  }
  MFREE3(10, 8, 4, x);
  MFREE3(N1, N2, N3, a);
  b = (T*)CALLOC3(N1, N2, N3, sizeof(T));
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        b[MINDEX3(N1, N2, N3, i, j, k)] = 1;
      }
    }
  }
  MFREE3(N1, N2, N3, b);
  int z = 0;
  return 0;
}
