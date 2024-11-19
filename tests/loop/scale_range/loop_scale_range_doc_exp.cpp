#include <optitrust.h>

void f() {
  __pure();
  __ghost(to_prove, "F := __is_neq(4, 0)");
  for (int j = 0; j < 4 * 10; j += 4) {
    __strict();
    __ghost(assume, "F := in_range(exact_div(j, 4), 0..10)");
    __ghost([&]() { __requires("in_range(exact_div(j, 4), 0..10)"); }, "");
    int x = exact_div(j, 4);
  }
}
