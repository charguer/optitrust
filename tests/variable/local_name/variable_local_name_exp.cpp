#include <optitrust.h>

typedef int T;

void ok1() {
__pure();
  T a;
      for (int j = 0; j < 10; j++) {
      __sequentially_modifies("&a ~> Cell");
    auto x = a;
        for (int i = 0; i < j; i++) {
__sequentially_modifies("&x ~> Cell");
          x++;
        }
        a = x;
      }
}

void ok2() {
  __pure();
  T a;
  auto x = a;
l : { x++; }
  a = x;
  int y = 0;
}

void ko1() {
  __pure();
  T a;
  int& b = a;
  for (int j = 0; j < 10; j++) {
    __sequentially_modifies("&a ~> Cell");
        for (int i = 0; i < j; i++) {
      __sequentially_modifies("&a ~> Cell");
      a++;
      b++;
    }
  }
  int y = 0;
  }

void ko2() {
  __pure();
  T a;
  int& b = a;
l : {
  a++;
  b++;
}
  int y = 0;
}

void ko_scope() {
  __pure();
  T x;
  T a;
auto x1 = a;
l : { x1++; }
a = x1;
}
