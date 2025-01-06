#include <optitrust.h>

int main () {
  const int N0 = 1;
  int* const a = CALLOC1(int, N0);
  for (int i = 0; i < 10; i++){
    a[MINDEX1(N0, i)];
  }
}
