#include <optitrust.h>
#include <stdlib.h>

int main() {
  const int N1 = 10;
  int* p = (int*)malloc(MSIZE1(N1) * sizeof(int));
  for (int i = 0; i < N1; i++) {
    if (10 <= i && i < 12) {
      i++;
    }
  }
  return 0;
}

int main2() {
  const int N1 = 10;
  const int N2 = 10;
  int* p = (int*)malloc(MSIZE2(N1, N2) * sizeof(int));
  for (int i = 0; i < N2; i++) {
    if (10 <= i && i < 12) {
      i++;
    }
  }
}
