#include <stdlib.h>

#include "../../include/optitrust.h"

typedef int T;

int main() {
  const int N = 10;
  int* p = (int*)MALLOC_ALIGNED1(N, sizeof(int), 64);
  T* q;
  q = (T*)MALLOC_ALIGNED3(N, N, N, sizeof(T), 64);
  return 0;
}
