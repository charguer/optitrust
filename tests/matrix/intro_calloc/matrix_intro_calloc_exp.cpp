#include <optitrust.h>
#include <stdlib.h>

int* q;

const int N = 10;

void allocate() { q = (int *)CALLOC1(N, sizeof(int)); }

int main() {
  int *p = (int *)CALLOC1(N, sizeof(int));
  return 0;
}
