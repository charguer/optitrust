#include <optitrust.h>

void f() {
  __pure();

  int a = 3;
  int b = 4;
  a += b;
}
