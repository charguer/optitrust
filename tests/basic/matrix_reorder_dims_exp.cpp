#include <stdlib.h>

#include "../../include/optitrust.h"

int main() {
  int i1 = 0, i2 = 0, i3 = 0;
  int const N1 = 10;
  int const N2 = 10;
  int const N3 = 10;
  int *p = (int *)MCALLOC3(N3, N2, N1, sizeof(int));
  p[MINDEX3(N3, N2, N1, i3, i2, i1)];
  return 0;
}
