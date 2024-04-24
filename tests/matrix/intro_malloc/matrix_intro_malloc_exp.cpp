#include <optitrust.h>
#include <stdlib.h>

const int N = 10;

int *q;

void allocate() { q = (int *)MALLOC1(N, sizeof(int)); }

int main() {
  int *p = (int *)MALLOC1(N, sizeof(int));
  return 0;
}
