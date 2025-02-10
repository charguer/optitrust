#include <optitrust.h>

int main() {
  int* const a = MALLOC1(int, 10);
  int* const b = MALLOC1(int, 8);
  for (int i = 0; i < 10; i++) {
     a[MINDEX1(10, i)] = i;
    if (i >= 2) {
      b[MINDEX1(8, i - 2)] = a[MINDEX1(10, i - 2)] +
                             a[MINDEX1(10, i - 1)] +
                             a[MINDEX1(10, i)];
    }
  }

  return 0;
}
