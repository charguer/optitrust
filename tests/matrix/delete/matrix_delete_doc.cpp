
#include "../../../include/optitrust.h"

int main() {
  int* const a = (int* const) MALLOC1(10, sizeof(int));
  MFREE1(10, a);
  return 0;
}
