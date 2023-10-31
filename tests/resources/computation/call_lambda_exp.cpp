#include "../../../include/optitrust.h"

__ghost_ret freeze_cell() {
  __requires("p: loc");
  __consumes("p ~> Cell");
  __produces("p ~> FrozenCell");
  __admitted();
}

__ghost_ret unfreeze_cell() {
  __reverts(freeze_cell);
  __admitted();
}

void f() {
  __pure();
  [&]() { __pure(); }();
  int x = 0;
  int y = 0;
  [&](int* a) {
    __modifies("a ~> Cell");
    __modifies("&x ~> Cell");
    x += 1;
    *a += 1;
  }(&y);
  const __ghost_fn pair =
      __ghost_begin(__with_reverse(
                        [&]() {
                          __consumes("&x ~> Cell");
                          __produces("&x ~> FrozenCell");
                          __ghost(freeze_cell, "p := &x");
                        },
                        [&]() { __ghost(unfreeze_cell, "p := &x"); }),
                    "");
  __ghost_end(pair);
}
