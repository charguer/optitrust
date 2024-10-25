#include <optitrust.h>

void f() {
  __pure();
  for (int j = 0; j < 4 * 10; j += 4) {
    __strict();
    __ghost(assume, "F := in_range(exact_div(j, 4), 0..10)");
    __ghost([&]() { __requires("in_range(exact_div(j, 4), 0..10)"); }, "");
    int x = exact_div(j, 4);
  }
}
