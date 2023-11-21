#include <optitrust.h>

void mut_var(int x) {
  __pure();

  const int y = 3;
  int z = y;
  int t = z;
  z = x;
  z += x;
  // implicit free
}

void uninit() {
  __pure();

  int x = 0;
  __ghost(forget_init, "&x ~> Cell");
  int a = 0;
}
