#include <optitrust.h>

__GHOST(freeze_cell) {
  __requires("T: Type, p: ptr(T)");
  __consumes("p ~> Cell");
  __produces("for _ in 0..1 -> p ~> Cell");
  __admitted();
}

__GHOST(unfreeze_cell) {
  __reverts(freeze_cell);
  __admitted();
}

void f(float* M, int n) {
  __modifies("for i in 0..n -> &M[i] ~> Cell");

  for (int i = 0; i < n; ++i) {
    __strict();
    __xconsumes("&M[i] ~> Cell");
    __xproduces("for _ in 0..1 -> &M[i] ~> Cell");
    __ghost(freeze_cell, "p := &M[i]");
  }
  for (int i = 0; i < n; ++i) {
    __strict();
    __xconsumes("for _ in 0..1 -> &M[i] ~> Cell");
    __xproduces("&M[i] ~> Cell");
    __ghost(unfreeze_cell, "p := &M[i]");
  }
}

