#include <optitrust.h>

void f() {
  __pure();
  __ghost(to_prove, "P := __is_true(4 != 0)");
  for (int j = 0; j < 4 * 10; j += 4) {
    __strict();
    __ghost(assume, "P := in_range(exact_div(j, 4), 0..10)");
    __ghost([&]() { __requires("in_range(exact_div(j, 4), 0..10)"); }, "");
    int x = exact_div(j, 4);
  }
}
