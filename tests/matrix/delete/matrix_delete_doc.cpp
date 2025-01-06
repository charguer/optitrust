#include <optitrust.h>

int main() {
  int* const a = MALLOC1(int, 10);
  free(a);
  return 0;
}
