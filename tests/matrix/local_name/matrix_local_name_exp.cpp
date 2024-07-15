#include <optitrust.h>

typedef int T;

T* b;

int main() {
  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* const a = (T* const)CALLOC3(N1, N2, N3, sizeof(T));
  T* const x = (T*)MALLOC3(N1, N2, N3, sizeof(T));
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
  int z = 0;
  return 0;
}

void f(int* c, int n) {
  __modifies("c ~> Matrix1(n)");
  int* const z = (int*)MALLOC1(n, sizeof(int));
  for (int i1 = 0; i1 < n; i1++) {
    z[MINDEX1(n, i1)] = c[MINDEX1(n, i1)];
  }
  for (int i = 0; i < n; i++) {
    __strict();
    __xmodifies("&z[MINDEX1(n, i)] ~> Cell");
    z[MINDEX1(n, i)] = i;
  }
  for (int i1 = 0; i1 < n; i1++) {
    c[MINDEX1(n, i1)] = z[MINDEX1(n, i1)];
  }
  MFREE1(n, z);
}
