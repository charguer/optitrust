#include <optitrust.h>

void f() {
  __pure();

  const int x = 3; /* TODO: non const x */
  const int y = x + 2;
  int z = y + y;
  int a = y + 3;
  z = y + 2;
  z = y + y;
}
