#include <optitrust.h>

void f() {
  __pure();
  __ghost(to_prove, "P := __is_true(5 != 0)");
  int x = 2 * 5;
  int y = exact_div(x, 5);
  x = y * 5;
}
