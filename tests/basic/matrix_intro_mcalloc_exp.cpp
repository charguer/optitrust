#include <stdlib.h>

#include "../../include/optitrust.h"

int main() {
  const int N = 10;
  int *p = (int *)MCALLOC1(N, sizeof(int));
  int *q = (int *)MCALLOC1(N, sizeof(int));
  return 0;
}