#include <optitrust.h>

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

void f(float* M, int n) {
  __modifies("for i in 0..n -> &M[i] ~> Cell");
  __ghost(
      [&]() {
        __consumes("for i in 0..n -> &M[i] ~> Cell");
        __produces("for i in 0..n -> &M[i] ~> FrozenCell");
        for (int i = 0; i < n; ++i) {
          __strict();
          __xconsumes("&M[i] ~> Cell");
          __xproduces("&M[i] ~> FrozenCell");
          __ghost(freeze_cell, "p := &M[i]");
        }
      },
      "");
  int x = 3;
  __ghost(
      [&]() {
        __consumes("for i in 0..n -> &M[i] ~> FrozenCell");
        __produces("for i in 0..n -> &M[i] ~> Cell");
        for (int i = 0; i < n; ++i) {
          __strict();
          __xconsumes("&M[i] ~> FrozenCell");
          __xproduces("&M[i] ~> Cell");
          __ghost(unfreeze_cell, "p := &M[i]");
        }
      },
      "");
}
