#include "../../../include/optitrust.h"

const int i0;

const int i1;

const int i2;

const int i3;

const int i4;

const int n0;

const int n1;

const int n2;

const int n3;

const int n4;

int main() {
  int* p;
  int* q = &p[i0 + i1];
  int r0 = MINDEX2(20, 30, 10, 0 + 20);
  int r1 = MINDEX3(10, 20, 30, 5, 0 + 10, 0 + 0 + 20);
  int r2 = MINDEX2(n0, n1, i0, i2 + i3);
  int r3 = MINDEX2(n0, n1, i0, i2) + MINDEX1(n0, i3);
}
