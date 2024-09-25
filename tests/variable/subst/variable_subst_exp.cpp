#include <optitrust.h>

void f() {
  __pure();
  const int x = 3;
  const int y = x + 2;
  int z = x + 2 + (x + 2);
  int a = 5 + 3;
  z = 5 + 2;
  z = 5 + 5;
}
