#include <optitrust.h>
#include <stdlib.h>

int main() {
  const int N = 10;
  int* p = (int*)MALLOC1((long unsigned int)N, sizeof(int));
  int* const x = (int*)MALLOC0(sizeof(int));
  MFREE0(x);
y_seq : {
  int* const y = (int*)MALLOC0(sizeof(int));
  y[MINDEX0()] = 0;
  MFREE0(y);
}
  return 0;
}
