#include <optitrust.h>

void f(int* t) {
  __modifies("Group(range(0, 10, 1), fun i -> Group(range(0, 10, 1), fun j -> &t[i] ~> Cell))");

  int a = 5;
  int b = 6;
  for (int i = 0; i < 10; i++) {
    __sequentially_reads("&a ~> Cell, &b ~> Cell");
    __modifies("Group(range(0, 10, 1), fun j -> &t[i] ~> Cell)");

    int r = i;
    for (int j = 0; j < 10; j++) {
      __sequentially_reads("&a ~> Cell, &b ~> Cell");
      __modifies("&t[i] ~> Cell");

      int s = i;
      int x = a + b;
      t[i] = i;
    }
  }
}
