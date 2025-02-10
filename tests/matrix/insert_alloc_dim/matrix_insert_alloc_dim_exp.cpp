#include <optitrust.h>
#include <stdlib.h>

int main() {
  const int N1 = 10;
  const int N2 = 20;
  int* p = (int*)malloc(MSIZE2(N2, N1) * sizeof(int));
  int* q = (int*)calloc(MSIZE2(N2, N1), sizeof(int));
  return 0;
}
