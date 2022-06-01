#include <stdlib.h>

#include "../../include/optitrust.h"

int main() {
  const int N1 = 10;
  const int N2 = 20;
  int *p = (int *)MALLOC1(N1, sizeof(int));
  int *q = (int *)CALLOC1(N1, sizeof(int));
  return 0;
}
