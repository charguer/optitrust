#include <optitrust.h>

int main() {
  int* const a = (int* const)MALLOC1(10, sizeof(int));
  int* const b = (int* const)MALLOC1(4, sizeof(int));
  for (int i1 = 3; i1 < 3 + 4; i1++) {
    b[MINDEX1(4, i1 - 3)] = a[MINDEX1(10, i1)];
  }
  for (int i = 3; i < 7; i++) {
    b[MINDEX1(4, i - 3)];
  }
  for (int i1 = 3; i1 < 3 + 4; i1++) {
    a[MINDEX1(10, i1)] = b[MINDEX1(4, i1 - 3)];
  }
  MFREE1(4, b);
  MFREE1(10, a);
}
