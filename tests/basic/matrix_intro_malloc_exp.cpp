#include <stdlib.h>

#include "../../include/optitrust.h"

int main() {
  const int N = 10;
  int* p = (int*)MALLOC1(N, sizeof(int));
  int* x = (int*)MALLOC0(sizeof(int));
  MFREE0(x);
y_seq : {
  int* y = (int*)MALLOC0(sizeof(int));
  y[MINDEX0()] = 0;
  MFREE0(y);
}
  return 0;
}
