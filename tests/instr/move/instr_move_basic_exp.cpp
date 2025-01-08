#include <optitrust.h>

void f() {
  __pure();
  int x = 3;
  int y = 1;
  int z = 5;
  x = y;
  const int v = 0;
  y = v;
  z = x;
  int t = 2;
  t = x;
}
