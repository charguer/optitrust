#include <optitrust.h>
#include <stdlib.h>

int main() {
  const int N1 = 10;
  const int N2 = 20;
  int* p = (int*)MALLOC3(N2, N2, N1, sizeof(int));
  int* q = (int*)CALLOC1(N1, sizeof(int));
  return 0;
}
