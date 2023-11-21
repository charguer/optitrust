#include <optitrust.h>

typedef int T;

T* b;

int main() {
  const int N0 = 1;
  T* a = (T*)CALLOC1(N0, sizeof(T));
  T* b = (T*)CALLOC1(N0, sizeof(T));
  for (int i1 = 0; i1 < N0; i1++) {
    b[MINDEX1(N0, i1)] = a[MINDEX1(N0, i1)];
  }
  for (int i = 0; i < 10; i++) {
    b[MINDEX1(N0, i)];
  }
  for (int i1 = 0; i1 < N0; i1++) {
    a[MINDEX1(N0, i1)] = b[MINDEX1(N0, i1)];
  }
  MFREE1(N0, b);
}
