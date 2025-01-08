#include <optitrust.h>

void f() {
  __pure();
  int x = 3;
  int y = 1;
  int z = 5;
  x = y;
  int t = 2;
  t = x;
  z = x;

  const int v = 0;
  y = v;
}
