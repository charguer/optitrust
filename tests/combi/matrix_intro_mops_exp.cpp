#include <stdlib.h>

int main() {
  int const N = 5;
  int *p = (int *)MCALLOC1(N, sizeof(int));
  for (int i = 0; (i < 5); i++) {
    p[MINDEX1(N, i)] = i;
  }
  int *q = (int *)MMALLOC1(N, sizeof(int));
  for (int i = 0; (i < 5); i++) {
    q[MINDEX1(N, i)] = i;
  }
  return 0;
}