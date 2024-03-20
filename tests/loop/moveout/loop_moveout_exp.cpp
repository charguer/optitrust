#include <optitrust.h>

void f(int* t) {
  __modifies("for i in 0..10 -> for j in 0..10 -> &t[i] ~> Cell");
  int a = 5;
  int b = 6;
  int x = a + b;
  for (int i = 0; i < 10; i++) {
    __strict();
    __writes("for j in 0..10 -> &t[i] ~> Cell");
    int r = i;
    int s = i;
    for (int j = 0; j < 10; j++) {
      __strict();
      __writes("&t[i] ~> Cell");
      t[i] = i;
    }
  }
}
