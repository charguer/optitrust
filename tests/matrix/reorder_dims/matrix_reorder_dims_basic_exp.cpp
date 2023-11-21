#include <optitrust.h>
#include <stdlib.h>

int main() {
  int i1 = 0, i2 = 0, i3 = 0;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  int *p = (int *)CALLOC3(N3, N2, N1, sizeof(int));
  p[MINDEX3(N3, N1, N2, i3, i1, i2)];
  return 0;
}
