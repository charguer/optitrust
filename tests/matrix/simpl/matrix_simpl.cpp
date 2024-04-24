#include <optitrust.h>

// TODO: handle non-const, but locally pure int
extern const int i0, i1, i2, i3, i4;
extern const int n0, n1, n2, n3, n4;

int main()
{
  int* p;
  int* q = &((&p[i0])[i1]); // = &p[i0 + i1]

  int r0 = MINDEX2(20, 30, 10, 0) + MINDEX1(30, 20);

  int r1 = MINDEX3(10, 20, 30, 5, 0, 0) + MINDEX2(20, 30, 10, 0) + MINDEX1(30, 20);
  // = MINDEX3(10, 20, 30, 5, 10, 20)

  int r2 = MINDEX2(n0, n1, i0, i2) + MINDEX1(n1, i3);
  // = MINDEX2(n0, n1, i0, i2 + i3)

  int r3 = MINDEX2(n0, n1, i0, i2) + MINDEX1(n0, i3);
  // dimensions are not matching
}

