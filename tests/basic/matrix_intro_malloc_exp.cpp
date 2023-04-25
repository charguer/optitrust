#include <stdlib.h>

#include "../../include/optitrust.h"

int main() {
  const int N = 10;
  int* p = (int*)MALLOC1(N, sizeof(int));
  int* x = (int*)MALLOC0(sizeof(int));
  free(x);
y_seq : {
  int* y = (int*)MALLOC0(sizeof(int));
  y[MINDEX0()] = 0;
  free(y);
}
  return 0;
}
