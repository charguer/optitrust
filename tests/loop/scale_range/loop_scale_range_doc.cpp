#include <optitrust.h>

void f() {
  __pure();

  for (int i = 0; i < 10; i++) {
    __ghost([&] {
      __requires("in_range(i, 0..10)");
    }, "");
    int x = i;
  }
}
