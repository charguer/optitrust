#include <optitrust.h>

void f(int* t) {
  __modifies("for i in 0..10 -> for j in 0..10 -> &t[i] ~> Cell");

  int a = 5;
  int b = 6;
  for (int i = 0; i < 10; i++) {
    __strict();
    __sreads("&a ~> Cell, &b ~> Cell");
    __xmodifies("for j in 0..10 -> &t[i] ~> Cell");

    int r = i;
    for (int j = 0; j < 10; j++) {
      __strict();
      __sreads("&a ~> Cell, &b ~> Cell");
      __xmodifies("&t[i] ~> Cell");

      int s = i;
      int x = a + b;
      t[i] = i;
    }
  }
}
