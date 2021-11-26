#include <stdlib.h>

#include "../../include/optitrust.h"

int main() {
  int const N = 10;
  int *p = (int *)MMALLOC1(N, sizeof(int));
  return 0;
}
