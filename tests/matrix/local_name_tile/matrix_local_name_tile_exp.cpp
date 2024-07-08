#include <optitrust.h>

typedef int T;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* const a = (T*)MALLOC3(10, 8, 4, sizeof(T));
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        a[MINDEX3(10, 8, 4, i, j - 2, k)] = 1;
      }
    }
  }
  MFREE3(10, 8, 4, a);
  for (int i = 0; i < 10; i++) {
    T* const y = (T*)MALLOC3(10, 8, 4, sizeof(T));
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        y[MINDEX3(10, 8, 4, i, j - 2, k)] = 1;
      }
    }
    MFREE3(10, 8, 4, y);
  }
  for (int i = 0; i < 10; i++) {
    T* const z = (T*)MALLOC3(1, 8, 4, sizeof(T));
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        z[MINDEX3(1, 8, 4, 0, j - 2, k)] = 1;
      }
    }
    MFREE3(1, 8, 4, z);
  }
  int z = 0;
  return 0;
}
