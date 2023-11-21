#include <optitrust.h>

typedef int T;

int main() {
  int const N0 = 5;
  int const N1 = 10;
  int const N2 = 11;
  int const N3 = 12;
  // This is a result of the local_name transformation
  T *a = (T *)CALLOC3(N1, N2, N3, sizeof(T));
   mark: {
    T *x = (T *)CALLOC3(N1, N2, N3, sizeof(T));
    for (int i1 = 0; (i1 < N1); i1++) {
      for (int i2 = 0; (i2 < N2); i2++) {
        for (int i3 = 0; (i3 < N3); i3++) {
          x[MINDEX3(N1, N2, N3, i1, i2, i3)] = a[MINDEX3(N1, N2, N3, i1, i2, i3)];
        }
      }
    }
    for (int i = 0; (i < 10); i++) {
      int t = x[MINDEX3(N1, N2, N3, i, (i + 1), (i + 2))];
      x[MINDEX3(N1, N2, N3, i, (i + 1), (i + 2))] = t + 2;
    }
    for (int i1 = 0; (i1 < N1); i1++) {
      for (int i2 = 0; (i2 < N2); i2++) {
        for (int i3 = 0; (i3 < N3); i3++) {
          a[MINDEX3(N1, N2, N3, i1, i2, i3)] = x[MINDEX3(N1, N2, N3, i1, i2, i3)];
        }
      }
    }
    MFREE(x);
   }
  int y = 0;
  return 0;
}
