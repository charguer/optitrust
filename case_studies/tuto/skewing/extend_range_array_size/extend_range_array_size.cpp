#include <optitrust.h>
#include <stdlib.h>

int main() {
  const int N1 = 10;
  const int *p = MALLOC1(int, N1);
  for (int i = 10; i < 12; i++) {
    i++;
  }
  return 0;
}
// Testing dim parameter
int main2() {
  const int N1 = 10;
  const int N2 = 10;
  const int *p = MALLOC2(int, N1, N2);
  for (int i = 10; i < 12; i++) {
    i++;
  }
}
