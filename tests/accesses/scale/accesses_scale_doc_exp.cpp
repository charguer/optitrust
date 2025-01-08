#include <optitrust.h>

void f() {
  __pure();
  int x = 2 * 5;
  int y = exact_div(x, 5);
  x = y * 5;
}
