#include <stdlib.h>

int main() {
  const int N = 5;
  int *p = (int *)CALLOC1(N, sizeof(int));
  for (int i = 1; i < 5; i++) {
    int a = p[MINDEX1(N, i - 1)];
    p[MINDEX1(N, i)] = i;
  }
  int *q = (int *)MALLOC1(N, sizeof(int));
  for (int i = 0; i < 5; i++) {
    q[MINDEX1(N, i)] = i;
  }
  return 0;
}
