#include <optitrust.h>

extern const int i0;

extern const int i1;

extern const int i2;

extern const int i3;

extern const int i4;

extern const int n0;

extern const int n1;

extern const int n2;

extern const int n3;

extern const int n4;

int main() {
  int* p;
  int* q = &p[i0 + i1];
  int r0 = MINDEX2(20, 30, 10, 0 + 20);
  int r1 = MINDEX3(10, 20, 30, 5, 0 + 10, 0 + 0 + 20);
  int r2 = MINDEX2(n0, n1, i0, i2 + i3);
  int r3 = MINDEX2(n0, n1, i0, i2) + MINDEX1(n0, i3);
}
