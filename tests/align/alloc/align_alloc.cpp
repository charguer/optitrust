#include <stdlib.h>

#include "../../../include/optitrust.h"

typedef int T;

int main() {
  const int N = 10;
  int *p = (int *) MALLOC1(N, sizeof(int));
  T* q;
  q = (T*) MALLOC3 (N, N, N, sizeof(T));

  return 0;
}
