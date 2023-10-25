#include "../../include/optitrust.h"

void f() {
  __pure();
  [&]() {}();
  int x = 0;
  int y = 0;
  [&](int* a) {
    x += 1;
    *a += 1;
  }(&y);
}
