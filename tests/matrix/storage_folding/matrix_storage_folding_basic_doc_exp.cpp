#include "../../../include/optitrust.h"

int main() {
  int* const a = (int* const)MALLOC1(3, sizeof(int));
  int* const b = (int* const)MALLOC1(8, sizeof(int));
  for (int i = 0; i < 10; i++) {
    a[MINDEX1(3, i % 3)] = i;
    if (i >= 2) {
      b[MINDEX1(8, i - 2)] = a[MINDEX1(3, (i - 2) % 3)] +
                             a[MINDEX1(3, (i - 1) % 3)] + a[MINDEX1(3, i % 3)];
    }
  }
  return 0;
}
