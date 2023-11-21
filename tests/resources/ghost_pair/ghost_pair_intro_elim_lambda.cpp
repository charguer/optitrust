#include <optitrust.h>

__GHOST(freeze_cell) {
  __requires("p: loc");
  __consumes("p ~> Cell");
  __produces("p ~> FrozenCell");
  __admitted();
}

__GHOST(unfreeze_cell) {
  __reverts(freeze_cell);
  __admitted();
}

void f(float* M, int n) {
  __modifies("Group(range(0,n,1), fun i -> &M[i] ~> Cell)");

  __GHOST_BEGIN_CUSTOM(pair,
    [&]{
      __consumes("Group(range(0,n,1), fun i -> &M[i] ~> Cell)");
      __produces("Group(range(0,n,1), fun i -> &M[i] ~> FrozenCell)");
      for (int i = 0; i < n; ++i) {
        __consumes("&M[i] ~> Cell");
        __produces("&M[i] ~> FrozenCell");
        __ghost(freeze_cell, "p := &M[i]");
      }
    },
    [&]{
      for (int i = 0; i < n; ++i) {
        __consumes("&M[i] ~> FrozenCell");
        __produces("&M[i] ~> Cell");
        __ghost(unfreeze_cell, "p := &M[i]");
      }
    });
  int x = 3;
  __GHOST_END(pair);
}

