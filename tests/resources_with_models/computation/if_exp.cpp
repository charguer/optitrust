#include <optitrust_models.h>

void f(int x) {
  __pure();
  int r;
  if (x == 0) {
    __ghost(assert_alias, "x := x, y := 0");
    r = 0;
  } else {
    r = x;
  }
}

void g(int* t, int i) {
  __writes("&t[i] ~~> 0");
  if (i == 0) {
    __ghost(assert_alias, "x := i, y := 0");
    t[0] = 0;
  } else {
    t[i] = 0;
  }
}

void h(int* t, int i) {
  __requires("v: int");
  __reads("&t[i] ~~> v");
  int x;
  if (i == 0) {
    __ghost(assert_alias, "x := i, y := 0");
    x = t[0];
  } else if (i == 1) {
    x = t[i];
  } else {
    x = t[i];
  }
}
