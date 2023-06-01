#include "../../include/optitrust.h"

typedef int T;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* a = (T*)MALLOC3(10, 8, 4, sizeof(T));
  for (int i = 0; i < 10; i++) {
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        a[MINDEX3(10, 8, 4, i - 0, j - 2, k - 0)] = 1;
      }
    }
  }
  free(a);
  for (int i = 0; i < 10; i++) {
    T* y = (T*)MALLOC3(10, 8, 4, sizeof(T));
    for (int j = 2; j < 10; j++) {
      for (int k = 0; k < 4; k++) {
        y[MINDEX3(10, 8, 4, i - 0, j - 2, k - 0)] = 1;
      }
    }
    free(y);
  }
  int z = 0;
  return 0;
}
