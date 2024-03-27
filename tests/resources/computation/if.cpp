#include <optitrust.h>

int f(int x) {
  __pure();

  int r;
  if (x == 0) {
    r = 0;
  } else {
    r = x;
  }
}

void g(int* t, int i) {
  __writes("&t[i] ~> Cell");

  if (i == 0) {
    __ghost(assert_alias, "i, 0");
    t[0] = 0;
  } else {
    t[i] = 0;
  }
}
