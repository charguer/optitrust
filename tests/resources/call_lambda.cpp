#include "../../include/optitrust.h"

void f() {
  __pure();

  [&]{ __pure(); }();

  int x = 0;
  int y = 0;
  [&](int* a){
    __modifies("a ~> Cell, &x ~> Cell");
    x += 1;
    (*a) += 1;
  }(&y);
}
