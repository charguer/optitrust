#include <optitrust.h>

void f(int* t) {
__modifies(
      "Group(range(0, 10, 1), fun i -> Group(range(0, 10, 1), fun j -> &t[i] "
      "~> Cell))");
  int a = 5;
  int b = 6;
  int x = a + b;
  for (int i = 0; i < 10; i++) {
__consumes("_Uninit(Group(range(0, 10, 1), fun j -> &t[i] ~> Cell))");
    __produces("Group(range(0, 10, 1), fun j -> &t[i] ~> Cell)");
    int r = i;
    int s = i;
    for (int j = 0; j < 10; j++) {
__consumes("_Uninit(&t[i] ~> Cell)");
      __produces("&t[i] ~> Cell");
      t[i] = i;
    }
  }
  }
